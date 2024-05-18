;;; dape-tests.el --- Tests for dape.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Free Software Foundation, Inc.

;; Author: Daniel Pettersson

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'dape)
(require 'ert)

;;; Helpers
(defun dape-test--goto-line (line)
  "Goto LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun dape-test--line-at-regex (regexp)
  "Search for a line matching the REGEXP in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((match (re-search-forward regexp nil t)))
      (when match
        (line-number-at-pos)))))

(defmacro dape-test--should (pred &optional seconds)
  "PRED should eventually be non nil during duration SECONDS.
If PRED does not eventually return nil, abort the current test as
failed."
  (let ((seconds (or seconds 10)))
    `(progn
       (with-timeout (,seconds)
         (while (not ,pred)
           (accept-process-output nil 0.01)))
       (should ,pred)
       (let ((ret ,pred))
         (ignore ret)
         ret))))

(defmacro dape-test--with-files (buffer-fixtures &rest body)
  "Setup BUFFER-FIXTURES and call BODY with bindings and clean up.
FIXTURE is an alist with form (BUFFER-BINDING (FILE-NAME . CONTENT-LIST).
CONTENT-LIST is an list of strings which are joined with newline to create file."
  (declare (indent 1) (debug t))
  `(dape-test--call-with-files ',(mapcar 'cdr buffer-fixtures)
                               (lambda ,(mapcar 'car buffer-fixtures)
                                 ,@body)))

(defvar dape-test--skip-cleanup nil
  "Skip `dape-test--call-with-files' cleanup.")

(defun dape-test--call-with-files (fixtures fn)
  "Setup FIXTURES and apply FN with created buffers.
Helper for `dape-test--with-files'."
  (let* ((temp-dir (make-temp-file "dape-tests-" t))
         (default-directory temp-dir)
         buffers)
    (unwind-protect
        (progn
          ;; init files and buffers
          (pcase-dolist (`(,file-name ,content) fixtures)
            (with-current-buffer (find-file-noselect file-name)
              (insert (mapconcat 'eval content "\n"))
              (save-buffer)
              (push (current-buffer) buffers)
              (goto-char (point-min))))
          (setq buffers (nreverse buffers))
          (apply fn buffers))
      ;; reset dape
      (unless dape-test--skip-cleanup
        (advice-add 'yes-or-no-p :around (defun always-yes (&rest _) t))
        (dape-quit)
        (unwind-protect
            ;; Post test asserts
            (progn
              (dape-test--should
               (not (dape--live-connection 'parent t)) 10)
              (dape-test--should
               (not (seq-find (lambda (buffer)
                                (and (not (equal (buffer-name buffer)
                                                 "*dape-connection events*"))
                                     (string-match-p "\\*dape-.+\\*"
                                                     (buffer-name buffer))))
                              (buffer-list))))
              (dape-test--should
               (not (process-list)) 10))
          (setq dape--connection nil)
          (advice-remove 'yes-or-no-p 'always-yes)
          (setq dape--info-expanded-p
                (make-hash-table :test 'equal))
          (setq dape--watched nil)
          (dolist (process (process-list))
            (delete-process process))
          (dolist (buffer buffers)
            (kill-buffer buffer))
          ;; clean up files
          (delete-directory temp-dir t))))))

(defun dape-test--apply-to-match (regex fn)
  "Apply FN to match of REGEX in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regex nil)
      (funcall-interactively fn))))

(defun dape-test--stopped-p ()
  "If current adapter connection is stopped."
  (dape--live-connection 'stopped t))

(defun dape-test--no-pending-p ()
  "If current session has 0 outgoing requests."
  (let ((parent (dape--live-connection 'parent t)))
    (or (not parent)
        (zerop (apply '+
                      (jsonrpc-continuation-count parent)
                      (mapcar 'jsonrpc-continuation-count (dape--children parent)))))))

(defun dape-test--should-stopped ()
  "Using `dape-test--should' assert that connection is stopped."
  (dape-test--should
   (and (dape-test--no-pending-p)
        (dape-test--stopped-p))))

(defun dape-test--revert-buffer ()
  "Revert buffer and wait for all pending request to resolve."
  (revert-buffer)
  (dape-test--should
   (dape-test--no-pending-p)))

(defun dape-test--debug (buffer key &rest args)
  "Invoke `dape' interactivly with KEY and ARGS."
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _)
               (concat (format "%s " key)
                       (mapconcat (lambda (o) (format "%S" o)) args " ")))))
    (with-current-buffer buffer
      (call-interactively 'dape))))

;;; Tests
(defun dape--test-restart (buffer key &rest args)
  "Helper for ert test `dape-test-restart'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape-test--debug buffer key args)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint")))
    ;; restart
    (goto-char (point-min))
    (dape-restart)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint")))))

(ert-deftest dape-test-restart ()
  "Restart with restart."
  (dape-test--with-files
   ((main-buffer
     "main.py"
     ("pass"
      "pass # breakpoint")))
   (dape--test-restart main-buffer 'debugpy))
  (dape-test--with-files
   ((index-buffer
     "index.js"
     ("()=>{};"
      "()=>{}; // breakpoint")))
   (dape--test-restart index-buffer 'js-debug-node))
  (dape-test--with-files
   ((index-buffer
     "main.c"
     ("int main() {"
      "  return 0; // breakpoint"
      "}")))
   (dape--test-restart index-buffer
                       'codelldb-cc
                       'compile "gcc -g -o a.out main.c"))
  (dape-test--with-files
   ((main-buffer
     "main.rb"
     ("puts \"\""
      "puts \"\""
      "0 # breakpoint")))
   (dape--test-restart main-buffer 'rdbg)))

(defun dape--test-restart-with-dape (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart-with-dape'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape-test--debug buffer dape-args)
    ;; at breakpoint and stopped
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint")))
    ;; restart
    (goto-char (point-min))
    (apply 'dape-test--debug buffer dape-args)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint")))))

(ert-deftest dape-test-restart-with-dape ()
  "Should be able to restart with `dape' even though session active."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("pass"
         "pass # breakpoint")))
    (dape--test-restart-with-dape main-buffer 'debugpy))
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    (dape--test-restart-with-dape index-buffer 'js-debug-node))
  (dape-test--with-files
      ((main-buffer
        "main.c"
        ("int main() {"
         "  return 0; // breakpoint"
         "}")))
    (dape--test-restart-with-dape main-buffer 'codelldb-cc
                                  'compile "gcc -g -o a.out main.c"))
  (dape-test--with-files
      ((main-buffer
        "main.rb"
        ("puts \"\""
         "puts \"\""
         "0 # breakpoint")))
    (dape--test-restart-with-dape main-buffer 'rdbg)))

(defun dape--test-scope-buffer (buffer &rest dape-args)
  "Helper for ert test `dape-test-scope-buffer-contents'.
Watch buffer should contain variables a, b and expandable c with
property member.
Expects line with string \"breakpoint\" in source."
  ;; set breakpoint
  (with-current-buffer buffer
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle))))
  (apply 'dape-test--debug buffer dape-args)
  ;; stopped and at breakpoint
  (dape-test--should-stopped)
  (with-current-buffer buffer
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint"))))
  (with-current-buffer (dape--info-get-buffer-create  'dape-info-scope-mode 0)
    (dape-test--revert-buffer)
    (dape-test--should
     (dape-test--line-at-regex "^  a"))
    (dape-test--should
     (dape-test--line-at-regex "^\\+ b"))
    ;; expansion
    (dape-test--apply-to-match "^\\+ b" 'dape-info-scope-toggle)
    (dape-test--should
     (dape-test--line-at-regex "^    member"))
    ;; contraction
    (dape-test--apply-to-match "^\\- b" 'dape-info-scope-toggle)
    (dape-test--should
     (not (dape-test--line-at-regex "^    member")))
    ;; set value
    (when (dape--capable-p (dape--live-connection 'parent t)
                           :supportsSetVariable)
      (dape-test--should
       (dape-test--line-at-regex "^  a *0"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "99")))
        (dape-test--apply-to-match "^  a" 'dape-info-variable-edit))
      (dape-test--should
       (dape-test--line-at-regex "^  a *99")))
    ;; add watch
    (dape-test--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-watch-mode)
      (dape-test--revert-buffer)
      (dape-test--should
       (dape-test--line-at-regex "^  a")))))

(ert-deftest dape-test-scope-buffer ()
  "Assert basic scope buffer content."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("class B:"
         "    member = 0"
         "a = 0"
         "b = B()"
         "pass # breakpoint")))
    (dape--test-scope-buffer main-buffer 'debugpy))
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("var a = 0;"
         "var b = {'member': 0};"
         "()=>{}; // breakpoint")))
    (dape--test-scope-buffer index-buffer 'js-debug-node))
  (dape-test--with-files
      ((main
        "main.c"
        ("int main() {"
         "  int a = 0;"
         "  struct { int member; } b = {0};"
         "  return 0; // breakpoint"
         "}")))
    (ignore main)
    (dape--test-scope-buffer main 'codelldb-cc
                             'compile "gcc -g -o a.out main.c")))

(ert-deftest dape-test-watch-buffer()
  "Watch buffer content and commands."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("class B:"
         "  member = 0"
         "a = 0"
         "b = B()"
         "pass # breakpoint")))
    ;; setup watched vars
    (dape-watch-dwim "a")
    (dape-watch-dwim "b")
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug main-buffer 'debugpy)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (with-current-buffer main-buffer
      (dape-test--should
       (equal (line-number-at-pos)
              (dape-test--line-at-regex "breakpoint"))))
    ;; contents of watch buffer
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-watch-mode)
      (dape-test--revert-buffer)
      (dape-test--should
       (and (dape-test--line-at-regex "^  a")
            (dape-test--line-at-regex "^\\+ b")))
      ;; expansion
      (dape-test--apply-to-match "^\\+ b" 'dape-info-scope-toggle)
      (dape-test--should
       (dape-test--line-at-regex "^    member"))
      ;; assert contraction
      (dape-test--apply-to-match "^\\- b" 'dape-info-scope-toggle)
      (dape-test--should
       (not (dape-test--line-at-regex "^    member")))
      ;; set value
      (dape-test--should
       (dape-test--line-at-regex "^  a *0"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "99")))
        (dape-test--apply-to-match "^  a *0" 'dape-info-variable-edit))
      (dape-test--revert-buffer)
      (dape-test--should
       (dape-test--line-at-regex "^  a *99"))
      ;; watch removal
      (dape-test--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
      (dape-test--revert-buffer)
      (dape-test--should
       (not (dape-test--line-at-regex "^  a"))))))

(ert-deftest dape-test-stack-buffer()
  "Stack buffer contents and commands."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("def a():"
         "    a_var = 0"
         "    b() # stack"
         "def b():"
         "    b_var = 0"
         "    pass # breakpoint"
         "a()")))
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug main-buffer 'debugpy)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "breakpoint"))))
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-stack-mode))
      ;; buffer contents
      (dape-test--should
       (and (dape-test--line-at-regex "^in b")
            (dape-test--line-at-regex "^in a")
            (member 'dape--info-stack-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-stack-position) 1)))
      ;; select stack frame
      (dape-test--apply-to-match "^in a" 'dape-info-stack-select)
      ;; buffer contents
      (dape-test--should
       (and (= (marker-position dape--info-stack-position)
               (save-excursion
                 (dape-test--goto-line (dape-test--line-at-regex "^in a"))
                 (point))))))
    ;; scope buffer should update to new stack
    (with-current-buffer
        (dape-test--should
         (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      (dape-test--should
       (dape-test--line-at-regex "^  a_var")))
    ;; source buffer points at new stack frame
    (with-current-buffer main-buffer
      (= (line-number-at-pos)
         (dape-test--line-at-regex "stack")))))

(ert-deftest dape-test-threads-buffer ()
  "Threads buffer contents and commands."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("import threading"
         "def thread_fn():"
         "    thread_var = 0"
         "    pass # breakpoint"
         "thread = threading.Thread(target=thread_fn)"
         "thread.start()"
         "thread.join()")))
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug main-buffer 'debugpy)
    ;; stopped and at breakpoint
    (dape-test--should-stopped)
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "breakpoint"))))
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-threads-mode)
      (dape-test--revert-buffer)
      ;; buffer contents
      (dape-test--should
       (and (dape-test--line-at-regex "^1 .* stopped in")
            (dape-test--line-at-regex "^2 .* stopped in thread_fn")
            (member 'dape--info-thread-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-thread-position)
               (save-excursion
                 (dape-test--goto-line (dape-test--line-at-regex
                                        "^2 .* stopped in thread_fn"))
                 (point))))))
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-scope-mode 0)
      (dape-test--revert-buffer)
      ;; scope buffer in thread_fn
      (dape-test--should
       (dape-test--line-at-regex "^  thread_var")))
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-threads-mode)
      ;; select thread
      (dape-test--apply-to-match "^1 .* stopped in" 'dape-info-select-thread))
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-threads-mode)
      (revert-buffer)
      ;; thread selected
      (dape-test--should
       (and (dape-test--line-at-regex "^1 .* stopped in")
            (dape-test--line-at-regex "^2 .* stopped in thread_fn")
            (member 'dape--info-thread-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-thread-position)
               (save-excursion
                 (dape-test--goto-line (dape-test--line-at-regex
                                        "^1 .* stopped in"))
                 (point))))))
    (with-current-buffer (dape--info-get-live-buffer 'dape-info-scope-mode 0)
      (revert-buffer)
      ;; scope buffer in thread_fn
      (dape-test--should
       (not (dape-test--line-at-regex "^  thread_var"))))))

(ert-deftest dape-test-repl-buffer ()
  "Repl buffer contents and commands."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("a = 0 # breakpoint"
         "b = 0 # second line"
         "c = 0 # third line")))
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug main-buffer 'debugpy)
    ;; stopped
    (dape-test--should-stopped)
    ;; at breakpoint
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "breakpoint"))))
    (pop-to-buffer "*dape-repl*")
    (insert "next")
    (comint-send-input)
    ;; stopped
    (dape-test--should-stopped)
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "second line"))))
    (insert "next")
    (comint-send-input)
    ;; stopped
    (dape-test--should-stopped)
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "third line"))))
    (insert "a = 99")
    (comint-send-input)
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      (dape-test--should
       (dape-test--line-at-regex "^  a *99")))))

(ert-deftest dape-test-modules-buffer ()
  "Modules buffer contents and commands."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("pass # breakpoint")))
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug main-buffer 'debugpy)
    ;; at breakpoint and stopped
    (dape-test--should-stopped)
    ;; contents
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-modules-mode)
      (revert-buffer)
      (dape-test--should ;; Regression .* symlinks are now handled differently
       (dape-test--line-at-regex "^__main__ of .*main.py")))))

(ert-deftest dape-test-sources-buffer ()
  "Sources buffer contents and commands."
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    ;; set breakpoint
    (with-current-buffer index-buffer
      (let ((line (dape-test--line-at-regex "breakpoint")))
        (save-excursion
          (dape-test--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape-test--debug index-buffer 'js-debug-node)
    ;; stopped
    (dape-test--should-stopped)
    ;; contents
    (with-current-buffer (dape--info-get-buffer-create 'dape-info-sources-mode)
      (revert-buffer)
      (dape-test--should (dape-test--line-at-regex "^os "))
      (dape-test--should (dape-test--line-at-regex "^util "))
      (dape-test--apply-to-match "^os " 'dape-info-sources-goto))
    (dape-test--should
     (member "*dape-source os*" (mapcar 'buffer-name (buffer-list))))))

(defun dape-test--breakpoint-hits (buffer key &rest args)
  "Helper for ert test `dape-test-breakpoint-hits'."
  (let ((breakpoints-buffer
         (dape--info-get-buffer-create 'dape-info-breakpoints-mode)))
    ;; No breakpoints in *dape-info Breakpoints*
    (with-current-buffer breakpoints-buffer
      (revert-buffer)
      (dape-test--should (not (dape-test--line-at-regex "index.js:2"))))
    ;; Set breakpoint in loop
    (with-current-buffer buffer
      (save-excursion
        (dape-test--goto-line (dape-test--line-at-regex "breakpoint"))
        (dape-breakpoint-toggle)))
    ;; Breakpoint in buffer
    (with-current-buffer breakpoints-buffer
      (revert-buffer)
      (dape-test--should (dape-test--line-at-regex "index.js:2")))
    ;; Start debugging
    (apply 'dape-test--debug buffer key args)
    ;; Continue 4 times
    (dotimes (_ 4)
      (dape-test--should-stopped)
      (dape-continue (dape--live-connection 'stopped)))
    ;; Debugging session over
    (dape-test--should-stopped)
    ;; Breakpoint in *dape-info Breakpoints*
    (with-current-buffer breakpoints-buffer
      (dape-test--revert-buffer)
      (dape-test--should
       (dape-test--line-at-regex "index.js:2 .*5 *$")))))

(ert-deftest dape-test-breakpoint-hits ()
  "Test breakpoint hits."
  (dape-test--with-files
   ((index-buffer
     "index.js"
     ("for (let i = 0; i < 5; i++) {"
      "()=>{}; // breakpoint"
      "}")))
   (dape-test--breakpoint-hits index-buffer 'js-debug-node)))

(provide 'dape-tests)
;;; dape-tests.el ends here
