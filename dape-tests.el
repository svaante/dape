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

(defun dape--lines-with-property (property &optional value)
  "All lines in current buffer with PROPERTY.
If VALUE line PROPERTY value `equal' VALUE."
  (save-excursion
    (goto-char (point-min))
    (let (line-numbers)
      (while (not (eobp))
        (let ((line-value (get-text-property (point) property)))
          (if (or (and (not value) line-value)
                  (and value (equal line-value value)))
              (push (line-number-at-pos) line-numbers))
          (forward-line 1)))
      (nreverse line-numbers))))

(defun dape--line-with-property (property &optional value)
  "Get first line in current buffer with PROPERTY.
If VALUE line PROPERTY value `equal' VALUE."
  (car (dape--lines-with-property property value)))

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
  `(dape--call-with-buffers ',(mapcar 'cadr buffer-fixtures)
                            (lambda ,(mapcar 'car buffer-fixtures)
                              ,@body)))

(defun dape--call-with-buffers (fixtures fn)
  "Setup FIXTURES and apply FN with created buffers.
Helper for `dape--with-buffers'."
  (let* ((temp-dir (make-temp-file "dape-tests-" t))
         (default-directory temp-dir)
         buffers)
    (unwind-protect
        (progn
          ;; init files and buffers
          (pcase-dolist (`(,file-name . ,content) fixtures)
            (with-current-buffer (find-file-noselect file-name)
              (insert (mapconcat 'eval content "\n"))
              (save-buffer)
              (push (current-buffer) buffers)
              ;; need prog mode for setting breakpoints
              ;; (prog-mode)
              ;; set normal breakpoints on 'bp
              (save-excursion
                (dolist (line (dape--lines-with-property 'bp))
                  (dape--goto-line line)
                  (dape-breakpoint-toggle)))
              ;; set condition breakpoints on 'condition
              (save-excursion
                (dolist (line (dape--lines-with-property 'condition))
                  (dape--goto-line line)
                  (dape-breakpoint-expression
                   (get-text-property (point) 'condition))))
              ;; set log breakpoints on 'log
              (save-excursion
                (dolist (line (dape--lines-with-property 'log))
                  (dape--goto-line line)
                  (dape-breakpoint-expression
                   (get-text-property (point) 'log))))
              (goto-char (point-min))))
          (setq buffers (nreverse buffers))
          (apply fn buffers))
      ;; reset dape
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
      (delete-directory temp-dir t))))

(defun dape--variable-names-in-buffer ()
  "Return list of variable names in buffer."
  (let (vars)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (thread-first (get-text-property (point) 'dape--info-variable)
                            (plist-get :name))
              vars)
        (forward-line 1)))
    (nreverse vars)))

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
Expects breakpoint bp 1 in source."
  (with-current-buffer buffer
    (goto-char (point-min))
    (apply 'dape- dape-args)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))
    ;; restart
    (goto-char (point-min))
    (dape-restart)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))))

(ert-deftest dape-test-restart ()
  "Restart with restart."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "pass"
                  (propertize "pass" 'bp 1))))
   (dape--test-restart main-buffer
                       'debugpy
                       :program (buffer-file-name main-buffer)
                       :cwd default-directory))
  (dape--with-buffers
   ((index-buffer ("index.js"
                   "()=>{};"
                   (propertize "()=>{};" 'bp 1))))
   (dape--test-restart index-buffer
                       'js-debug-node
                       :program (buffer-file-name index-buffer)
                       :cwd default-directory))
  (dape--with-buffers
   ((index-buffer ("main.c"
                   "int main() {"
                   (propertize "return 0;" 'bp 1)
                   "}")))
   (dape--test-restart index-buffer
                       'codelldb-cc
                       :program
                       (file-name-concat default-directory "./a.out")
                       :cwd default-directory
                       'compile "gcc -g -o a.out main.c"))
  (dape--with-buffers
      ((main-buffer ("main.rb"
                     "puts \"\""
                     "puts \"\""
                     (propertize "0" 'bp 1))))
       (dape--test-restart main-buffer
                           'rdbg
                           'command-cwd default-directory
                           '-c (format "ruby \"%s\"" (buffer-file-name main-buffer))))

  ;; (dape--with-buffers
  ;;  ((main-buffer (("main.go"
  ;;                 package main"
  ;;                 "func main() {"
  ;;                 (propertize "}" 'bp 1))))
  ;;  (dape--test-restart main-buffer
  ;;                      'dlv
  ;;                      :program default-directory
  ;;                      :cwd default-directory))
  )

(defun dape--test-restart-with-dape (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart-with-dape'.
Expects breakpoint bp 1 in source."
  (with-current-buffer buffer
    (goto-char (point-min))
    (apply 'dape- dape-args)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))
    ;; restart
    (goto-char (point-min))
    (apply 'dape- dape-args)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (and (eq dape--state 'stopped)
          (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))))

(ert-deftest dape-test-restart-with-dape ()
  "Should be able to restart with `dape' even though session active."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "pass"
                  (propertize "pass" 'bp 1))))
   (dape--test-restart-with-dape main-buffer
                                 'debugpy
                                 :program (buffer-file-name main-buffer)
                                 :cwd default-directory))
  (dape--with-buffers
      ((index-buffer ("index.js"
                      "()=>{};"
                      (propertize "()=>{};" 'bp 1))))
   (dape--test-restart-with-dape index-buffer
                                 'js-debug-node
                                 :program (buffer-file-name index-buffer)
                                 :cwd default-directory))
  (dape--with-buffers
   ((main-buffer ("main.c"
                  "int main() {"
                  (propertize "return 0;" 'bp 1)
                  "}")))
   (dape--test-restart-with-dape main-buffer
                                 'codelldb-cc
                                 :program
                                 (file-name-concat default-directory "./a.out")
                                 :cwd default-directory
                                 'compile "gcc -g -o a.out main.c"))
  (dape--with-buffers
      ((main-buffer ("main.rb"
                     "puts \"\""
                     "puts \"\""
                     (propertize "0" 'bp 1))))
       (dape--test-restart-with-dape main-buffer
                                     'rdbg
                                     'command-cwd default-directory
                                     '-c (format "ruby \"%s\"" (buffer-file-name main-buffer)))))

(defun dape--test-scope-buffer (buffer &rest dape-args)
  "Helper for ert test `dape-test-scope-buffer-contents'.
Watch buffer should contain variables a, b and expandable c with
property member.
Breakpoint should be present on a line where all variables are present."
  (apply 'dape- dape-args)
  ;; we are at breakpoint and stopped
  (with-current-buffer buffer
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1))))
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
   ((main-buffer ("main.py"
                  "class B:"
                  "\tmember = 0"
                  "a = 0"
                  "b = B()"
                  (propertize "pass" 'bp 1))))
   (dape--test-scope-buffer main-buffer
                            'debugpy
                            :program (buffer-file-name main-buffer)
                            :cwd default-directory))
  (dape--with-buffers
   ((index-buffer ("index.js"
                   "var a = 0;"
                   "var b = {'member': 0};"
                   (propertize "()=>{};" 'bp 1))))
   (dape--test-scope-buffer index-buffer
                            'js-debug-node
                            :program (buffer-file-name index-buffer)
                            :cwd default-directory))
  (dape--with-buffers
   ((main ("main.c"
           "int main() {"
           "int a = 0;"
           "struct { int member; } b = {0};"
           (propertize "return 0;" 'bp 1)
           "}")))
   (ignore main)
   (dape--test-scope-buffer main
                            'codelldb-cc
                            :program
                            (file-name-concat default-directory "./a.out")
                            :cwd default-directory
                            'compile "gcc -g -o a.out main.c")))

(ert-deftest dape-test-watch-buffer()
  "Assert basic watch buffer content and actions."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "class B:"
                  "\tmember = 0"
                  "a = 0"
                  "b = B()"
                  (propertize "pass" 'bp 1))))
  (dape-watch-dwim "a")
  (dape-watch-dwim "b")
  (dape- 'debugpy
          :program (buffer-file-name main-buffer)
          :cwd default-directory)
  ;; we are at breakpoint and stopped
  (with-current-buffer main-buffer
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1))))
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
  "Assert basic test buffer content and actions."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "def a():"
                  "    a_var = 0"
                  (propertize "    b()" 'stack 2)
                  "def b():"
                  "    b_var = 0"
                  (propertize "    pass" 'bp 1)
                  "a()")))
   (dape- 'debugpy
          :program (buffer-file-name main-buffer)
          :cwd default-directory)
   ;; we are at breakpoint and stopped
   (with-current-buffer main-buffer
     (dape--should-eventually
      (= (line-number-at-pos)
         (dape--line-with-property 'bp 1))))
   (with-current-buffer (dape--should-eventually
                         (dape--info-get-live-buffer 'dape-info-stack-mode))
    (dape--should-eventually
     (and (dape--line-number-at-regex "^1 in b")
          (dape--line-number-at-regex "^2 in a")
          (member 'dape--info-stack-position
                  overlay-arrow-variable-list)
          (= (marker-position dape--info-stack-position) 1)))

    (dape--apply-to-match "^2 in a" 'dape-info-stack-select)
    (dape--should-eventually
     (and (= (marker-position dape--info-stack-position)
             (save-excursion
               (dape--goto-line (dape--line-number-at-regex "^2 in a"))
               (point)))
          (with-current-buffer
              (dape--should-eventually
               (dape--info-get-live-buffer 'dape-info-scope-mode 0))
            (dape--line-number-at-regex "^  a_var"))
          (with-current-buffer main-buffer
            (= (line-number-at-pos)
               (dape--line-with-property 'stack 2))))))))



