;;; dape-tests.el --- Tests for dape.el            -*- lexical-binding: t; -*-

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

(defmacro dape--should-eventually (pred &optional seconds)
  "PRED should eventually be non nil during duration SECONDS.'
If PRED does not eventually return nil, abort the current test as
failed."
  (let ((seconds (or seconds 10)))
    `(progn
       (with-timeout (,seconds (should ,pred))
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
              (prog-mode)
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
      ;; clean up files
      (delete-directory temp-dir t)
      ;; reset dape
      (setq dape--info-expanded-p
            (make-hash-table :test 'equal))
      (setq dape--watched nil)
      (dape-quit)
      (dape--should-eventually (not dape--process) 10)
      (setq dape--state nil)
      (sleep-for 10) ;; UGNGHGHG
      )))

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

(defun dape--apply-to-matches (regex fn)
  "Apply FN to each match of REGEX in the current buffer."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq found t)
        (funcall-interactively fn)))
    (unless found
      (error "Pattern %S not found in %s" regex (current-buffer)))))

(defun dape- (key &rest options)
  "Invoke `dape' config KEY with OPTIONS."
  (dape (dape--config-eval key options)))

;;; Tests
(defun dape--test-restart (buffer &rest dape-args)
  "Helper for ert test `dape-test-restart'.
Expects breakpoint bp 1 in source."
  (apply 'dape- dape-args)
  (with-current-buffer buffer
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1)))
    ;; reset point to first line
    (goto-char (point-min))
    (dape--should-eventually
     (not (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))
    ;; restart
    (dape-restart)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1)))))

(ert-deftest dape-test-restart ()
  "Restart with debugpy restart."
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
  (apply 'dape- dape-args)
  (with-current-buffer buffer
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1)))
    ;; reset point to first line
    (goto-char (point-min))
    (dape--should-eventually
     (not (equal (line-number-at-pos)
                 (dape--line-with-property 'bp 1))))
    ;; rerun last session
    (apply 'dape- dape-args)
    ;; assert that we are at breakpoint
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1)))))

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
                                 'compile "gcc -g -o a.out main.c")))

(defun dape--test-scope-buffer-contents (buffer &rest dape-args)
  "Helper for ert test `dape-test-scope-buffer-contents'.
Watch buffer should contain variables a, b and expandable c with
property member.
Breakpoint should be present on a line where all variables are present."
  (apply 'dape- dape-args)
  ;; assert that we are at breakpoint and stopped
  (with-current-buffer buffer
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1))))
  ;; validate content
  (with-current-buffer (dape--should-eventually
                        (dape--info-get-live-buffer 'dape-info-scope-mode 0))
    (dape--should-eventually
     (member "a" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "b" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "c" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (not (member "member" (dape--variable-names-in-buffer))))
    (dape--apply-to-matches "^\\+ c" 'dape-info-scope-toggle)
    (dape--should-eventually
     (member "a" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "b" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "c" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "member" (dape--variable-names-in-buffer)))))

(ert-deftest dape-test-scope-buffer-contents ()
  "Assert basic scope buffer content."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "class C:"
                  "\tmember = 0"
                  "a = 0"
                  "b = 0"
                  "c = C()"
                  (propertize "pass" 'bp 1))))
   (dape--test-scope-buffer-contents main-buffer
                                     'debugpy
                                     :program (buffer-file-name main-buffer)
                                     :cwd default-directory))
  (dape--with-buffers
   ((index-buffer ("index.js"
                   "var a = 0;"
                   "var b = 0;"
                   "var c = {'member': 0};"
                   (propertize "()=>{};" 'bp 1))))
   (dape--test-scope-buffer-contents index-buffer
                                     'js-debug-node
                                     :program (buffer-file-name index-buffer)
                                     :cwd default-directory))
  (dape--with-buffers
   ((main ("main.c"
           "int main() {"
           "int a = 0;"
           "int b = 0;"
           "struct { int member; } c = {0};"
           (propertize "return 0;" 'bp 1)
           "}")))
   (ignore main)
   (dape--test-scope-buffer-contents main
                                     'codelldb-cc
                                     :program
                                     (file-name-concat default-directory "./a.out")
                                     :cwd default-directory
                                     'compile "gcc -g -o a.out main.c")))

(defun dape--test-watch-buffer-contents (buffer &rest dape-args)
  "Helper for ert test `dape-test-watch-buffer-contents'.
Watch buffer should contain variables a and expandable c with
property member.
Breakpoint should be present on a line where all variables are present."
  (dape-watch-dwim "a")
  (dape-watch-dwim "b")
  (apply 'dape- dape-args)
  ;; assert that we are at breakpoint and stopped
  (with-current-buffer buffer
    (dape--should-eventually
     (equal (line-number-at-pos)
            (dape--line-with-property 'bp 1))))
  (dape--should-eventually
   (equal dape--state "stopped"))
  ;; validate contents of watch buffer
  (with-current-buffer (dape--should-eventually
                        (dape--info-get-live-buffer 'dape-info-watch-mode))
    (dape--should-eventually
     (member "a" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "b" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (not (member "member" (dape--variable-names-in-buffer))))
    (dape--apply-to-matches "^\\+ b" 'dape-info-scope-toggle)
    (dape--should-eventually
     (member "a" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "b" (dape--variable-names-in-buffer)))
    (dape--should-eventually
     (member "member" (dape--variable-names-in-buffer)))))

(ert-deftest dape-test-watch-buffer-contents ()
  "Assert basic watch buffer content."
  (dape--with-buffers
   ((main-buffer ("main.py"
                  "class C:"
                  "\tmember = 0"
                  "a = 0"
                  "b = C()"
                  (propertize "pass" 'bp 1))))
   (dape--test-watch-buffer-contents main-buffer
                                     'debugpy
                                     :program (buffer-file-name main-buffer)
                                     :cwd default-directory)))
