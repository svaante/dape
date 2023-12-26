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
(defun dape--goto-line (line)
  "Goto LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun dape--line-number-at-regex (regexp)
  "Search for a line matching the REGEXP in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((match (re-search-forward regexp nil t)))
      (when match
        (line-number-at-pos)))))

(defmacro dape--should-eventually (pred &optional seconds)
  "PRED should eventually be non nil during duration SECONDS.'
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

(defmacro dape--with-buffers (buffer-fixtures &rest body)
  "Setup BUFFER-FIXTURES call body with bindings and clean up.
FIXTURE is an alist of the form
(BUFFER-BINDING (FILE-NAME . CONTENT-LIST).

Inserts breakpoints based on string properties in elements in
CONTENT-LIST.
- bp: inserts breakpoint
- condition: inserts condition breakpoint
- log: inserts log breakpoint"
  (declare (indent 1) (debug t))
  `(dape--call-with-buffers ',(mapcar 'cdr buffer-fixtures)
                            (lambda ,(mapcar 'car buffer-fixtures)
                              ,@body)))

(defvar dape--test-skip-cleanup nil)

(defun dape--call-with-buffers (fixtures fn)
  "Setup FIXTURES and apply FN with created buffers.
Helper for `dape--with-buffers'."
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
      (unless dape--test-skip-cleanup
        (advice-add 'yes-or-no-p :around (defun always-yes (&rest _) t))
        (dape-quit)
        (setq dape--info-expanded-p
              (make-hash-table :test 'equal))
        (setq dape--watched nil)
        (dape--should-eventually
         (not dape--process) 10)
        (dape--should-eventually
         (not (seq-find (lambda (buffer)
                          (string-match-p "\\*dape-.+\\*"
                                          (buffer-name buffer)))
                        (buffer-list))))
        (advice-remove 'yes-or-no-p 'always-yes)
        ;; clean up files
        (delete-directory temp-dir t)))))

(defun dape--apply-to-match (regex fn)
  "Apply FN to match of REGEX in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regex nil)
      (funcall-interactively fn))))

(defun dape- (key &rest options)
  "Invoke `dape' config KEY with OPTIONS."
  (dape (dape--config-eval key options)))

;;; Tests
(defun dape--test-restart (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape--line-number-at-regex "breakpoint")))
      (save-excursion
        (dape--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape- dape-args)
    ;; at breakpoint and stopped
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-number-at-regex "breakpoint"))))
    ;; restart
    (goto-char (point-min))
    (dape-restart)
    ;; at breakpoint and stopped
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-number-at-regex "breakpoint"))))))

(ert-deftest dape-test-restart ()
  "Restart with restart."
  (dape--with-buffers
      ((main-buffer
        "main.py"
        ("pass"
         "pass # breakpoint")))
    (dape--test-restart main-buffer
                        'debugpy
                        :program (buffer-file-name main-buffer)
                        :cwd default-directory))
  (dape--with-buffers
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    (dape--test-restart index-buffer
                        'js-debug-node
                        :program (buffer-file-name index-buffer)
                        :cwd default-directory))
  (dape--with-buffers
      ((index-buffer
        "main.c"
        ("int main() {"
         "  return 0; // breakpoint"
         "}")))
    (dape--test-restart index-buffer
                        'codelldb-cc
                        :program
                        (file-name-concat default-directory "./a.out")
                        :cwd default-directory
                        'compile "gcc -g -o a.out main.c"))
  (dape--with-buffers
      ((main-buffer
        "main.rb"
        ("puts \"\""
         "puts \"\""
         "0 # breakpoint")))
    (dape--test-restart main-buffer
                        'rdbg
                        'command-cwd default-directory
                        '-c (format "ruby \"%s\"" (buffer-file-name main-buffer)))))

(defun dape--test-restart-with-dape (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart-with-dape'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape--line-number-at-regex "breakpoint")))
      (save-excursion
        (dape--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape- dape-args)
    ;; at breakpoint and stopped
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-number-at-regex "breakpoint"))))
    ;; restart
    (goto-char (point-min))
    (apply 'dape- dape-args)
    ;; at breakpoint and stopped
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-number-at-regex "breakpoint"))))))

(ert-deftest dape-test-restart-with-dape ()
  "Should be able to restart with `dape' even though session active."
  (dape--with-buffers
      ((main-buffer
        "main.py"
        ("pass"
         "pass # breakpoint")))
    (dape--test-restart-with-dape main-buffer
                                  'debugpy
                                  :program (buffer-file-name main-buffer)
                                  :cwd default-directory))
  (dape--with-buffers
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    (dape--test-restart-with-dape index-buffer
                                  'js-debug-node
                                  :program (buffer-file-name index-buffer)
                                  :cwd default-directory))
  (dape--with-buffers
      ((main-buffer
        "main.c"
        ("int main() {"
         "  return 0; // breakpoint"
         "}")))
    (dape--test-restart-with-dape main-buffer
                                  'codelldb-cc
                                  :program
                                  (file-name-concat default-directory "./a.out")
                                  :cwd default-directory
                                  'compile "gcc -g -o a.out main.c"))
  (dape--with-buffers
      ((main-buffer
        "main.rb"
        ("puts \"\""
         "puts \"\""
         "0 # breakpoint")))
    (dape--test-restart-with-dape main-buffer
                                  'rdbg
                                  'command-cwd default-directory
                                  '-c (format "ruby \"%s\"" (buffer-file-name main-buffer)))))

(defun dape--test-scope-buffer (buffer &rest dape-args)
  "Helper for ert test `dape-test-scope-buffer-contents'.
Watch buffer should contain variables a, b and expandable c with
property member.
Expects line with string \"breakpoint\" in source."
  ;; set breakpoint
  (with-current-buffer buffer
    (let ((line (dape--line-number-at-regex "breakpoint")))
      (save-excursion
        (dape--goto-line line)
        (dape-breakpoint-toggle))))
  (apply 'dape- dape-args)
  ;; we are at breakpoint and stopped
  (with-current-buffer buffer
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-number-at-regex "breakpoint"))))
  (with-current-buffer (dape--should-eventually
                        (dape--info-get-live-buffer 'dape-info-scope-mode 0))
    (dape--should-eventually
     (dape--line-number-at-regex "^  a"))
    (dape--should-eventually
     (dape--line-number-at-regex "^\\+ b"))
    ;; expansion
    (dape--apply-to-match "^\\+ b" 'dape-info-scope-toggle)
    (dape--should-eventually
     (dape--line-number-at-regex "^    member"))
    ;; contraction
    (dape--apply-to-match "^\\- b" 'dape-info-scope-toggle)
    (dape--should-eventually
     (not (dape--line-number-at-regex "^    member")))
    ;; set value
    (when (eq (plist-get dape--capabilities :supportsSetVariable)
              t)
      (dape--should-eventually
       (dape--line-number-at-regex "^  a *0"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "99")))
        (dape--apply-to-match "^  a" 'dape-info-variable-edit))
      (dape--should-eventually
       (dape--line-number-at-regex "^  a *99")))
    ;; add watch
    (dape--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-watch-mode))
      (dape--should-eventually
       (dape--line-number-at-regex "^  a")))))

(ert-deftest dape-test-scope-buffer ()
  "Assert basic scope buffer content."
  (dape--with-buffers
      ((main-buffer
        "main.py"
        ("class B:"
         "    member = 0"
         "a = 0"
         "b = B()"
         "pass # breakpoint")))
    (dape--test-scope-buffer main-buffer
                             'debugpy
                             :program (buffer-file-name main-buffer)
                             :cwd default-directory))
  (dape--with-buffers
      ((index-buffer
        "index.js"
        ("var a = 0;"
         "var b = {'member': 0};"
         "()=>{}; // breakpoint")))
    (dape--test-scope-buffer index-buffer
                             'js-debug-node
                             :program (buffer-file-name index-buffer)
                             :cwd default-directory))
  (dape--with-buffers
      ((main
        "main.c"
        ("int main() {"
         "  int a = 0;"
         "  struct { int member; } b = {0};"
         "  return 0; // breakpoint"
         "}")))
    (ignore main)
    (dape--test-scope-buffer main
                             'codelldb-cc
                             :program
                             (file-name-concat default-directory "./a.out")
                             :cwd default-directory
                             'compile "gcc -g -o a.out main.c")))

(ert-deftest dape-test-watch-buffer()
  "Watch buffer content and commands."
  (dape--with-buffers
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
      (let ((line (dape--line-number-at-regex "breakpoint")))
        (save-excursion
          (dape--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape- 'debugpy
           :program (buffer-file-name main-buffer)
           :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape--should-eventually
       (equal (line-number-at-pos)
              (dape--line-number-at-regex "breakpoint"))))
    (dape--should-eventually
     (equal dape--state 'stopped))
    ;; contents of watch buffer
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-watch-mode))
      (dape--should-eventually
       (and (dape--line-number-at-regex "^  a")
            (dape--line-number-at-regex "^\\+ b")))
      ;; expansion
      (dape--apply-to-match "^\\+ b" 'dape-info-scope-toggle)
      (dape--should-eventually
       (dape--line-number-at-regex "^    member"))
      ;; assert contraction
      (dape--apply-to-match "^\\- b" 'dape-info-scope-toggle)
      (dape--should-eventually
       (not (dape--line-number-at-regex "^    member")))
      ;; set value
      (dape--should-eventually
       (dape--line-number-at-regex "^  a *0"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "99")))
        (dape--apply-to-match "^  a" 'dape-info-variable-edit))
      (dape--should-eventually
       (dape--line-number-at-regex "^  a *99"))
      ;; watch removal
      (dape--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
      (dape--should-eventually
       (not (dape--line-number-at-regex "^  a"))))))

(ert-deftest dape-test-stack-buffer()
  "Stack buffer contents and commands."
  (dape--with-buffers
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
      (let ((line (dape--line-number-at-regex "breakpoint")))
        (save-excursion
          (dape--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape- 'debugpy
           :program (buffer-file-name main-buffer)
           :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape--should-eventually
       (= (line-number-at-pos)
          (dape--line-number-at-regex "breakpoint"))))
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-stack-mode))
      ;; buffer contents
      (dape--should-eventually
       (and (dape--line-number-at-regex "^1 in b")
            (dape--line-number-at-regex "^2 in a")
            (member 'dape--info-stack-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-stack-position) 1)))
      ;; select stack frame
      (dape--apply-to-match "^2 in a" 'dape-info-stack-select)
      ;; buffer contents
      (dape--should-eventually
       (and (= (marker-position dape--info-stack-position)
               (save-excursion
                 (dape--goto-line (dape--line-number-at-regex "^2 in a"))
                 (point))))))
    ;; scope buffer should update to new stack
    (with-current-buffer
        (dape--should-eventually
         (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      (dape--should-eventually
       (dape--line-number-at-regex "^  a_var")))
    ;; source buffer points at new stack frame
    (with-current-buffer main-buffer
      (= (line-number-at-pos)
         (dape--line-number-at-regex "stack")))))

(ert-deftest dape-test-threads-buffer ()
  "Threads buffer contents and commands."
  (dape--with-buffers
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
      (let ((line (dape--line-number-at-regex "breakpoint")))
        (save-excursion
          (dape--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape- 'debugpy
           :program (buffer-file-name main-buffer)
           :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape--should-eventually
       (= (line-number-at-pos)
          (dape--line-number-at-regex "breakpoint"))))
    (dape--info-buffer 'dape-info-threads-mode)
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
      ;; buffer contents
      (dape--should-eventually
       (and (dape--line-number-at-regex "^1 .* stopped in")
            (dape--line-number-at-regex "^2 .* stopped in thread_fn")
            (member 'dape--info-thread-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-thread-position)
               (save-excursion
                 (dape--goto-line (dape--line-number-at-regex
                                   "^2 .* stopped in thread_fn"))
                 (point))))))
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      ;; scope buffer in thread_fn
      (dape--should-eventually
       (dape--line-number-at-regex "^  thread_var")))
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
      ;; select thread
      (dape--apply-to-match "^1 .* stopped in" 'dape-info-select-thread))
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
      ;; thread selected
      (dape--should-eventually
       (and (dape--line-number-at-regex "^1 .* stopped in")
            (dape--line-number-at-regex "^2 .* stopped in thread_fn")
            (member 'dape--info-thread-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-thread-position)
               (save-excursion
                 (dape--goto-line (dape--line-number-at-regex
                                   "^1 .* stopped in"))
                 (point))))))
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      ;; scope buffer in thread_fn
      (dape--should-eventually
       (not (dape--line-number-at-regex "^  thread_var"))))))



(ert-deftest dape-test-repl-buffer ()
  "Threads buffer contents and commands."
  (dape--with-buffers
      ((main-buffer
        "main.py"
        ("a = 0 # breakpoint"
         "b = 0 # second line"
         "c = 0 # third line")))
    ;; set breakpoint
    (with-current-buffer main-buffer
      (let ((line (dape--line-number-at-regex "breakpoint")))
        (save-excursion
          (dape--goto-line line)
          (dape-breakpoint-toggle))))
    ;; start debugging
    (dape- 'debugpy
           :program (buffer-file-name main-buffer)
           :cwd default-directory)
    ;; at breakpoint and stopped
    (dape--should-eventually
     (eq dape--state 'stopped))
    (with-current-buffer main-buffer
      (dape--should-eventually
       (= (line-number-at-pos)
          (dape--line-number-at-regex "breakpoint"))))
    (pop-to-buffer "*dape-repl*")
    (insert "next")
    (comint-send-input)
    (with-current-buffer main-buffer
      (dape--should-eventually
       (= (line-number-at-pos)
          (dape--line-number-at-regex "second line"))))
    (insert "next")
    (comint-send-input)
    (with-current-buffer main-buffer
      (dape--should-eventually
       (= (line-number-at-pos)
          (dape--line-number-at-regex "third line"))))
    (insert "a = 99")
    (comint-send-input)
    (with-current-buffer (dape--should-eventually
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      (dape--should-eventually
       (dape--line-number-at-regex "^  a *99")))))
