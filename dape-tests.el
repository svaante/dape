;;; dape-tests.el --- Tests for dape.el            -*- lexical-binding: t; -*-

;;; Code:
(require 'dape)
(require 'ert)

(setq ert-batch-print-level 10)
(setq ert-batch-print-length 15)

;;; Helpers
(defun dape--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun dape--lines-with-property (property &optional value)
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
  (car (dape--lines-with-property property value)))

(defmacro dape--with-files (file-fixtures &rest body)
  `(let* ((temp-dir (make-temp-file "dape-tests-" t))
          (default-directory temp-dir)
          (buffers (buffer-list)))
     (unwind-protect
         (progn
           (setq dape--info-expanded-p
                 (make-hash-table :test 'equal))
           (dape-quit)
           (dape--should-eventually (not dape--process))
           ,@(mapcar (pcase-lambda (`(,file-name . ,content))
                       `(with-current-buffer (find-file-noselect ,file-name)
                          (insert (mapconcat 'eval ',content "\n"))
                          (save-buffer)
                          ;; Set normal breakpoints
                          (save-excursion
                            (dolist (line (dape--lines-with-property 'bp))
                              (dape--goto-line line)
                              (dape-breakpoint-toggle))
                            (goto-char (point-min)))
                          ;; Set condition breakpoints
                          (save-excursion
                            (dolist (line (dape--lines-with-property 'condition))
                              (dape--goto-line line)
                              (dape-breakpoint-expression
                               (get-text-property (point) 'condition))))
                          (goto-char (point-min))
                          (goto-char (point-min))))
                     file-fixtures)
           ,@body)
       (delete-directory temp-dir t)
       (dolist (buffer (buffer-list))
         (unless (member buffer buffers)
           (kill-buffer buffer))))))

(defmacro dape--should-eventually (pred &optional seconds)
  (let ((seconds (or seconds 5)))
    `(progn
       (with-timeout (,seconds (should ,pred))
         (while (not ,pred)
           (accept-process-output nil 1)))
       (should ,pred)
       ,pred)))

(defun dape--variables-in-buffer ()
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
  "Apply function FN to each match of REGEX in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (funcall-interactively fn))))

(defun dape- (key &rest options)
  (dape (dape--config-eval key options)))

;;; Tests

(ert-deftest dape-test-debugpy-scope-buffer ()
  (dape--with-files
   (("main.py"
     "a = 0"
     "b = 'test'"
     "c = {'a': [1]}"
     (propertize "pass" 'bp 1)))
   (dape- 'debugpy
          :program (expand-file-name "main.py")
          :cwd (expand-file-name "."))
   (dape--should-eventually
    (equal dape--state "stopped"))
   ;; Validate content
   (with-current-buffer (dape--should-eventually
                         (dape--info-get-live-buffer 'dape-info-scope-mode 0))
     (dape--should-eventually
      (equal
       (dape--variables-in-buffer)
       '("special variables" "a" "b" "c")))
     (dape--apply-to-matches "^+ c" 'dape-info-scope-toggle)
     (dape--should-eventually
      (equal
       (dape--variables-in-buffer)
       '("special variables" "a" "b" "c"
         "special variables" "function variables" "'a'" "len()"))))))

