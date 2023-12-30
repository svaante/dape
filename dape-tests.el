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

(defmacro dape-test--with-files (buffer-fixtures &rest body)
  "Setup BUFFER-FIXTURES call body with bindings and clean up.
FIXTURE is an alist of the form
(BUFFER-BINDING (FILE-NAME . CONTENT-LIST).

Inserts breakpoints based on string properties in elements in
CONTENT-LIST.
- bp: inserts breakpoint
- condition: inserts condition breakpoint
- log: inserts log breakpoint"
  (declare (indent 1) (debug t))
  `(dape-test--call-with-files ',(mapcar 'cdr buffer-fixtures)
                               (lambda ,(mapcar 'car buffer-fixtures)
                                 ,@body)))

(defvar dape--test-skip-cleanup nil)

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
      (unless dape--test-skip-cleanup
        (advice-add 'yes-or-no-p :around (defun always-yes (&rest _) t))
        (dape-quit)
        (setq dape--info-expanded-p
              (make-hash-table :test 'equal))
        (setq dape--watched nil)
        (dape-test--should
         (not dape--process) 10)
        (dape-test--should
         (not (seq-find (lambda (buffer)
                          (string-match-p "\\*dape-.+\\*"
                                          (buffer-name buffer)))
                        (buffer-list))))
        (dape-test--should
         (not (process-list)))
        (advice-remove 'yes-or-no-p 'always-yes)
        ;; clean up files
        (delete-directory temp-dir t)))))

(defun dape-test--apply-to-match (regex fn)
  "Apply FN to match of REGEX in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regex nil)
      (funcall-interactively fn))))

(defun dape-test--debug (key &rest options)
  "Invoke `dape' config KEY with OPTIONS."
  (dape (dape--config-eval key options)))

;;; Tests
(defun dape--test-restart (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape-test--debug dape-args)
    ;; at breakpoint and stopped
    (dape-test--should
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape-test--line-at-regex "breakpoint"))))
    ;; restart
    (goto-char (point-min))
    (dape-restart)
    ;; at breakpoint and stopped
    (dape-test--should
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape-test--line-at-regex "breakpoint"))))))

(ert-deftest dape-test-restart ()
  "Restart with restart."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("pass"
         "pass # breakpoint")))
    (dape--test-restart main-buffer
                        'debugpy
                        :program (buffer-file-name main-buffer)
                        :cwd default-directory))
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    (dape--test-restart index-buffer
                        'js-debug-node
                        :program (buffer-file-name index-buffer)
                        :cwd default-directory))
  (dape-test--with-files
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
  (dape-test--with-files
      ((main-buffer
        "main.rb"
        ("puts \"\""
         "puts \"\""
         "0 # breakpoint")))
    (dape--test-restart main-buffer
                        'rdbg
                        'command-cwd default-directory
                        '-c (format "ruby \"%s\""
                                    (buffer-file-name main-buffer)))))

(defun dape--test-restart-with-dape (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart-with-dape'.
Expects line with string \"breakpoint\" in source."
  (with-current-buffer buffer
    ;; set breakpoint
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle)))
    (apply 'dape-test--debug dape-args)
    ;; at breakpoint and stopped
    (dape-test--should
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape-test--line-at-regex "breakpoint"))))
    ;; restart
    (goto-char (point-min))
    (apply 'dape-test--debug dape-args)
    ;; at breakpoint and stopped
    (dape-test--should
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape-test--line-at-regex "breakpoint"))))))

(ert-deftest dape-test-restart-with-dape ()
  "Should be able to restart with `dape' even though session active."
  (dape-test--with-files
      ((main-buffer
        "main.py"
        ("pass"
         "pass # breakpoint")))
    (dape--test-restart-with-dape main-buffer
                                  'debugpy
                                  :program (buffer-file-name main-buffer)
                                  :cwd default-directory))
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("()=>{};"
         "()=>{}; // breakpoint")))
    (dape--test-restart-with-dape index-buffer
                                  'js-debug-node
                                  :program (buffer-file-name index-buffer)
                                  :cwd default-directory))
  (dape-test--with-files
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
  (dape-test--with-files
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
    (let ((line (dape-test--line-at-regex "breakpoint")))
      (save-excursion
        (dape-test--goto-line line)
        (dape-breakpoint-toggle))))
  (apply 'dape-test--debug dape-args)
  ;; we are at breakpoint and stopped
  (with-current-buffer buffer
    (dape-test--should
     (equal (line-number-at-pos)
            (dape-test--line-at-regex "breakpoint"))))
  (with-current-buffer (dape-test--should
                        (dape--info-get-live-buffer 'dape-info-scope-mode 0))
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
    (when (eq (plist-get dape--capabilities :supportsSetVariable)
              t)
      (dape-test--should
       (dape-test--line-at-regex "^  a *0"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "99")))
        (dape-test--apply-to-match "^  a" 'dape-info-variable-edit))
      (dape-test--should
       (dape-test--line-at-regex "^  a *99")))
    ;; add watch
    (dape-test--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-watch-mode))
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
    (dape--test-scope-buffer main-buffer
                             'debugpy
                             :program (buffer-file-name main-buffer)
                             :cwd default-directory))
  (dape-test--with-files
      ((index-buffer
        "index.js"
        ("var a = 0;"
         "var b = {'member': 0};"
         "()=>{}; // breakpoint")))
    (dape--test-scope-buffer index-buffer
                             'js-debug-node
                             :program (buffer-file-name index-buffer)
                             :cwd default-directory))
  (dape-test--with-files
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
    (dape-test--debug 'debugpy
                      :program (buffer-file-name main-buffer)
                      :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape-test--should
       (equal (line-number-at-pos)
              (dape-test--line-at-regex "breakpoint"))))
    (dape-test--should
     (equal dape--state 'stopped))
    ;; contents of watch buffer
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-watch-mode))
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
        (dape-test--apply-to-match "^  a" 'dape-info-variable-edit))
      (dape-test--should
       (dape-test--line-at-regex "^  a *99"))
      ;; watch removal
      (dape-test--apply-to-match "^  a" 'dape-info-scope-watch-dwim)
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
    (dape-test--debug 'debugpy
                      :program (buffer-file-name main-buffer)
                      :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "breakpoint"))))
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-stack-mode))
      ;; buffer contents
      (dape-test--should
       (and (dape-test--line-at-regex "^1 in b")
            (dape-test--line-at-regex "^2 in a")
            (member 'dape--info-stack-position
                    overlay-arrow-variable-list)
            (= (marker-position dape--info-stack-position) 1)))
      ;; select stack frame
      (dape-test--apply-to-match "^2 in a" 'dape-info-stack-select)
      ;; buffer contents
      (dape-test--should
       (and (= (marker-position dape--info-stack-position)
               (save-excursion
                 (dape-test--goto-line (dape-test--line-at-regex "^2 in a"))
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
    (dape-test--debug 'debugpy
                      :program (buffer-file-name main-buffer)
                      :cwd default-directory)
    ;; at breakpoint and stopped
    (with-current-buffer main-buffer
      (dape-test--should
       (= (line-number-at-pos)
          (dape-test--line-at-regex "breakpoint"))))
    (dape--info-buffer 'dape-info-threads-mode)
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
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
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
      ;; scope buffer in thread_fn
      (dape-test--should
       (dape-test--line-at-regex "^  thread_var")))
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
      ;; select thread
      (dape-test--apply-to-match "^1 .* stopped in" 'dape-info-select-thread))
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-threads-mode))
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
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-scope-mode 0))
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
    (dape-test--debug 'debugpy
                      :program (buffer-file-name main-buffer)
                      :cwd default-directory)
    ;; at breakpoint and stopped
    (dape-test--should
     (eq dape--state 'stopped))
    (with-current-buffer main-buffer
      (dape-test--should
       (and (= (line-number-at-pos)
               (dape-test--line-at-regex "breakpoint"))
            (eq dape--state 'stopped))))
    (pop-to-buffer "*dape-repl*")
    (insert "next")
    (comint-send-input)
    (with-current-buffer main-buffer
      (dape-test--should
       (and (= (line-number-at-pos)
               (dape-test--line-at-regex "second line"))
            (eq dape--state 'stopped))))
    (insert "next")
    (comint-send-input)
    (with-current-buffer main-buffer
      (dape-test--should
       (and (= (line-number-at-pos)
               (dape-test--line-at-regex "third line"))
            (eq dape--state 'stopped))))
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
    (dape-test--debug 'debugpy
                      :program (buffer-file-name main-buffer)
                      :cwd default-directory)
    ;; at breakpoint and stopped
    (dape-test--should
     (eq dape--state 'stopped))
    (dape--info-buffer 'dape-info-modules-mode)
    ;; contents
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-modules-mode))
      (dape-test--should
       (dape-test--line-at-regex "^__main__ of main.py")))))

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
    (dape-test--debug 'js-debug-node
                      :program (buffer-file-name index-buffer)
                      :cwd default-directory)
    ;; stopped
    (dape-test--should
     (eq dape--state 'stopped))
    (dape--info-buffer 'dape-info-sources-mode)
    ;; contents
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-sources-mode))
      (dape-test--should
       (and (dape-test--line-at-regex "^os ")
            (dape-test--line-at-regex "index.js"))))
    (with-current-buffer (dape-test--should
                          (dape--info-get-live-buffer 'dape-info-sources-mode))
      (dape-test--apply-to-match "^os " 'dape-info-sources-goto))
    (dape-test--should
     (member "*dape-source os*" (mapcar 'buffer-name (buffer-list))))))
