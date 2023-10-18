;;; dape.el -- Debug Adapter Protocol for Emacs -*- lexical-binding: t -*-

;; Author: Daniel Pettersson
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/svaante/dape
;; Package-Requires: ((emacs "29.1"))

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

;; This package is an debug adapter client for Emacs.
;; Use `dape-configs' to set up your debug adapter configurations.

;; To initiate debugging sessions, use the command `dape'.

;; Note:
;; For complete functionality, it's essential to activate `eldoc-mode'
;; in your source buffers and enable `repeat-mode' for ergonomics

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'font-lock)
(require 'pulse)
(require 'comint)
(require 'repeat)
(require 'compile)
(require 'tree-widget)
(require 'project)


;;; Custom
(defgroup dape nil
  "Debug Adapter Protocol for Emacs."
  :prefix "dape-"
  :group 'applications)

(defcustom dape-configs nil
  "This variable holds the Dape configurations as an alist.
In this alist, the car element serves as a symbol identifying each
configuration.  Each configuration, in turn, is a property list (plist)
where keys can be symbols or keywords.

Symbol Keys (Used by Dape):
- command: Shell command to initiate the debug adapter.
- command-args: List of string arguments for the command.
- command-cwd: Working directory for the command.
- host: Host of the debug adapter.
- port: Port of the debug adapter.
- modes: List of modes where the configuration is active in `dape'
  completions.
- compile: Executes a shell command with `dape-compile-fn'.

Debug adapter connection in configuration:
- If only command is specified (without host and port), Dape
  will communicate with the debug adapter through stdin/stdout.
- If both host and port are specified, Dape will connect to the
  debug adapter.  If `command is specified, Dape will wait until the
  command is initiated before it connects with host and port.

Keywords in configuration:
  Keywords are transmitted to the adapter during the initialize and
  launch/attach requests.  Refer to `json-serialize' for detailed
  information on how Dape serializes these keyword elements.  Dape
  uses nil as false.

Functions and symbols in configuration:
 If a value in a key is a function, the function's return value will
 replace the key's value before execution.
 If a value in a key is a symbol, the symbol will recursively resolve
 at runtime."
  :type '(alist :key-type (symbol :tag "Name")
                :value-type
                (plist :options
                       (((const :tag "List of modes where config is active in `dape' completions" modes) (repeat function))
                        ((const :tag "Shell command to initiate the debug adapter" command) (choice string symbol))
                        ((const :tag "List of string arguments for command" command-args) (repeat string))
                        ((const :tag "Working directory for command" command-cwd) (choice string symbol))
                        ((const :tag "Host of debug adapter" host) string)
                        ((const :tag "Port of debug adapter" port) natnum)
                        ((const :tag "Compile cmd" compile) string)
                        ((const :tag "Adapter type" :type) string)
                        ((const :tag "Request type launch/attatch" :request) string)))))

(defcustom dape-key-prefix "\C-x\C-a"
  "Prefix of all dape commands."
  :type 'key)

(defcustom dape-on-start-hooks '(dape-info dape-repl)
  "Hook to run on session start.
The hook is run with one argument, the compilation buffer."
  :type 'hook)

(defcustom dape-inline-variables nil
  "Show variable values inline."
  :type 'boolean)

(defcustom dape-main-functions '("main")
  "Functions to set breakpoints at startup if no other breakpoints are set."
  :type '(repeat string))

(defcustom dape-info-buttons
  '(("next" . dape-next)
    ("step" . dape-step-in)
    ("out" . dape-step-out)
    ("cont" . dape-continue)
    ("pause" . dape-pause)
    ("restart" . dape-restart)
    ("quit" . dape-quit))
  "Actions to be displayed in `dape-info' buffer."
  :type '(alist :key-type string
                :value-type function))

(defcustom dape-read-memory-default-count 1024
  "The default count for `dape-read-memory'."
  :type 'natnum)

(defcustom dape-repl-commands
  '(("debug" . dape)
    ("next" . dape-next)
    ("continue" . dape-continue)
    ("pause" . dape-pause)
    ("step" . dape-step-in)
    ("out" . dape-step-out)
    ("restart" . dape-restart)
    ("kill" . dape-kill)
    ("quit" . dape-quit))
  "Dape commands available in REPL buffer."
  :type '(alist :key-type string
                :value-type function))

(defcustom dape-compile-fn #'compile
  "Function to run compile with."
  :type 'function)

(defcustom dape-cwd-fn #'dape--default-cwd
  "Function to get current working directory."
  :type 'function)

(defcustom dape-compile-compile-hooks nil
  "Hook run after dape compilation succeded.
The hook is run with one argument, the compilation buffer."
  :type 'hook)

(defcustom dape-repl-use-shorthand nil
  "Dape `dape-repl-commands' can be invokend with first char of command."
  :type 'boolean)

(defcustom dape-inline-variable-length 30
  "Maximum length of inline variable overlays."
  :type 'natnum)

(defcustom dape--debug-on
  '(io info error std-server)
  "Types of logs should be printed to *dape-debug*."
  :type '(set (const :tag "dap IO" io)
              (const :tag "info logging" info)
              (const :tag "error logging" error)
              (const :tag "dap tcp server stdout" std-server)))

;;; Face

(defface dape-log-face
  '((t :inherit (font-lock-doc-face)
       :height 0.85 :box (:line-width -1)))
  "Face used to display log breakpoints.")

(defface dape-expression-face
  '((t :inherit (font-lock-warning-face)
       :height 0.85 :box (:line-width -1)))
  "Face used to display conditional breakpoints.")

(defface dape-breakpoint-face
  '((t :inherit (bold)))
  "Face used to display breakpoint overlays.")

(defface dape-stack-trace
  '((t :inherit highlight :extend t))
  "Face used to display stack trace overlays.")

(defface dape-repl-exit-code-exit
  '((t :inherit compilation-mode-line-exit :extend t))
  "Face used in repl for exit code 0.")

(defface dape-repl-exit-code-fail
  '((t :inherit compilation-mode-line-fail :extend t))
  "Face used in repl for non 0 exit codes.")


;;; Vars

(defvar dape--name nil
  "Current session `dape-config' identifier.")
(defvar dape--config nil
  "Current session configuration plist.")
(defvar dape--timers nil
  "List of running timers.")
(defvar dape--seq nil
  "Session seq number.")
(defvar dape--seq-event nil
  "Session event seq number.")
(defvar dape--cb nil
  "Hash table of request callbacks.")
(defvar dape--state nil
  "Session state string.")
(defvar dape--thread-id nil
  "Selected thread id.")
(defvar dape--stack-id nil
  "Selected stack id.")
(defvar dape--capabilities nil
  "Session capabilities plist.")
(defvar dape--threads nil
  "Session plist of thread data.")
(defvar dape--stack-pointers nil
  "List of session stack pointer overlays.")
(defvar dape--breakpoints nil
  "List of session breakpoint overlays.")
(defvar dape--variable-overlays nil
  "List of variaiable overlays.")
(defvar dape--exceptions nil
  "List of available exceptions as plists.")
(defvar dape--watched nil
  "List of watched expressions.")
(defvar dape--server-process nil
  "Debug adapter server process.")
(defvar dape--process nil
  "Debug adapter communications process.")
(defvar dape--parent-process nil
  "Debug adapter parent process.  Used for by startDebugging adapters.")

(defvar dape--tree-widget-open-p (make-hash-table :test 'equal)
  "Hash table of open `dape--tree-widget' widgets.")

(defvar dape--scopes-widget nil
  "Scope widget in *dape-info* buffer.")
(defvar dape--watched-widget nil
  "Watched widget in *dape-info* buffer.")
(defvar dape--stack-widget nil
  "Stack widget in *dape-info* buffer.")
(defvar dape--threads-widget nil
  "Threads widget in *dape-info* buffer.")
(defvar dape--breakpoints-widget nil
  "Breakpoints widget in *dape-info* buffer.")
(defvar dape--exceptions-widget nil
  "Exceptions widget in *dape-info* buffer.")

(defvar dape--widget-guard nil
  "Guard var for *dape-info* buffer widget updates.")
(defvar dape--repl-insert-text-guard nil
  "Guard var for *dape-repl* buffer text updates.")

(defvar dape--config-history nil
  "History of used dape configs.  See `dape--read-config'.")


;;; Utils

(defmacro dape--callback (&rest body)
  "Ergonomics for `dape-request' callback."
  `(lambda (&optional process body success msg)
     (ignore process body success msg)
     ,@body))

(defmacro dape--with (request-fn args &rest body)
  "Ergonomics for `dape-request'."
  (declare (indent 2))
  `(,request-fn ,@args (dape--callback ,@body)))

(defun dape--next-like-command (command &optional arg)
  "Helper for interactive step like commands.
Run step like COMMAND.  If ARG is set run COMMAND ARG times."
  (if (dape--stopped-threads)
      (dotimes (_ (or arg 1))
        (dape-request (dape--live-process)
                      command
                      (dape--thread-id-object)
                      (dape--callback
                       (when success
                         (dape--remove-stack-pointers)
                         (dolist (thread dape--threads)
                           (plist-put thread :status "running"))
                         (dape--info-update-threads-widget)
                         (dape--update-state "running")))))
    (message "No stopped thread.")))

(defun dape--thread-id-object ()
  "Helper to construct a thread id object."
  (when dape--thread-id
    (list :threadId dape--thread-id)))

(defun dape--stopped-threads ()
  "List of stopped threads."
  (mapcan (lambda (thread)
            (when (equal (plist-get thread :status) "stopped")
              (list thread)))
          dape--threads))

(defun dape--current-thread ()
  "Current thread plist."
  (seq-find (lambda (thread)
              (eq (plist-get thread :id) dape--thread-id))
            dape--threads))

(defun dape--current-stack-frame ()
  "Current stack frame plist."
  (let* ((stack-frames (thread-first
                         (dape--current-thread)
                         (plist-get :stackFrames)))
         (stack-frames-with-source
          (seq-filter (lambda (stack-frame)
                        (thread-first stack-frame
                                      (plist-get :source)
                                      (plist-get :path)))
                      stack-frames)))
    (or (seq-find (lambda (stack-frame)
                    (eq (plist-get stack-frame :id)
                        dape--stack-id))
                  stack-frames-with-source)
        (car stack-frames-with-source)
        (car stack-frames))))

(defun dape--object-to-marker (plist &optional buffer-open-fn)
  "Create marker from dap PLIST containing file and line information.
If BUFFER-OPEN-FN is set, use that function to open a buffer from file path."
  (when-let* ((path (thread-first plist
                                  (plist-get :source)
                                  (plist-get :path)))
              (line (plist-get plist :line))
              (buffer-open-fn (or buffer-open-fn 'find-file-noselect))
              (buffer (funcall buffer-open-fn path)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (when-let ((column (plist-get plist :column)))
          (when (> column 0)
            (forward-char (1- column))))
        (point-marker)))))

(defun dape--goto-source (plist &optional no-select pulse)
  "Goto file and line of dap PLIST containing file and line information.
If NO-SELECT does not select buffer.
If PULSE pulse on after opening file."
  (when-let ((marker (dape--object-to-marker plist)))
    (let ((window
           (display-buffer (marker-buffer marker)
                           '(display-buffer-reuse-window
                             display-buffer-pop-up-window))))
      (unless no-select
        (select-window window))
      (with-current-buffer (marker-buffer marker)
        (with-selected-window window
          (goto-char (marker-position marker))
          (when pulse
            (pulse-momentary-highlight-region (line-beginning-position)
                                              (line-beginning-position 2)
                                              'next-error)))))))

(defun dape--default-cwd ()
  "Try to guess current project absolute file path."
  (expand-file-name
   (or (when-let ((project (project-current)))
         (project-root project))
       default-directory)))

(defun dape-find-file (&optional default)
  "Read filename without any ignored extensions at project root.
DEFAULT specifies which file to return on empty input."
  (let ((completion-ignored-extensions nil)
        (default-directory (funcall dape-cwd-fn)))
    (expand-file-name
     (read-file-name (if default
                         (format "Program (default %s): " default)
                       "Program : ")
                     default-directory
                     default t))))

(defun dape-find-file-buffer-default ()
  "Read filename at project root, defaulting to current buffer."
  (dape-find-file (buffer-file-name)))

(defun dape--overlay-region (&optional extended)
  "List of beg and end of current line.
If EXTENDED end of line is after newline."
  (list (line-beginning-position)
        (if extended
            (line-beginning-position 2)
          (1- (line-beginning-position 2)))))

(defun dape--variable-string (plist)
  "Formats dap variable PLIST to string."
  (let ((name (plist-get plist :name))
        (value (or (plist-get plist :value)
                   (plist-get plist :result)))
        (type (plist-get plist :type)))
    (concat
     (propertize name
                 'face 'font-lock-variable-name-face)
     (unless (or (null value)
                 (string-empty-p value))
       (format " = %s"
               (propertize value
                           'face 'font-lock-number-face)))
     (unless (or (null type)
                 (string-empty-p type))
       (format ": %s"
               (propertize type
                           'face 'font-lock-type-face))))))

(defun dape--format-file-line (file line)
  "Formats FILE and LINE to string."
  (concat (string-trim-left
           file
           (regexp-quote
            (expand-file-name
             (plist-get dape--config :cwd))))
          (when line
            (format ":%d"
                    line))))


;;; Process and parsing

;; HACK Issue #1 for some reason \r is not inserted into the parse
;;      buffer by codelldb on windows. No trace in source code.
(defconst dape--content-length-re
  "Content-Length: \\([[:digit:]]+\\)\r?\n\r?\n"
  "Matches debug adapter protocol header.")

(defun dape--debug (type string &rest objects)
  "Prints STRING of TYPE to *dape-debug*.
See `format' for STRING and OBJECTS usage.
See `dape-debug-on' for TYPE information."
  (when (memq type dape--debug-on)
    (with-current-buffer (get-buffer-create "*dape-debug*")
      (setq buffer-read-only t)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (concat (propertize (format "[%s]"
                                            (symbol-name type))
                                    'face 'match)
                        " "
                        (apply 'format string objects))
               "\n")))))

(defun dape--live-process (&optional nowarn)
  "Get current live process.
If NOWARN does not error on no active process."
  (if (and dape--process
           (processp dape--process)
           (process-live-p dape--process))
      dape--process
    (unless nowarn
      (user-error "No debug process live"))))

(defun dape--process-sentinel (process _msg)
  "Sentinel for dape processes."
  (unless (process-live-p process)
    (dape--remove-stack-pointers)
    (dape--debug 'info "\nProcess %S exited with %d"
                 (process-command process)
                 (process-exit-status process))))

(defun dape--handle-object (process object)
  "Handle a incoming parsed OBJECT from PROCESS."
  (dape--debug 'io "Received:\n%s" (pp-to-string object))
  (when-let* ((type-string (plist-get object :type))
              (type (intern type-string)))
    (cl-case type
      (response
       (let ((seq (plist-get object :request_seq)))
         (when-let ((timer (gethash seq dape--timers)))
           (cancel-timer timer)
           (remhash seq dape--timers))
         (when-let ((cb (gethash seq dape--cb)))
           (funcall cb
                    process
                    (plist-get object :body)
                    (plist-get object :success)
                    (plist-get object :message))
           (remhash seq dape--cb))))
      (request
        (run-with-timer 0 nil 'dape-handle-request
                        process
                        (intern (plist-get object :command))
                        (plist-get object :arguments)))
      (event
       (let ((seq (plist-get object :seq)))
         (cond
          ;; FIXME This is only here for `godot' which keeps sending duplicate.
          ((or (> seq dape--seq-event)
               (zerop seq))
           (setq dape--seq-event seq)
           (dape-handle-event process
                              (intern (plist-get object :event))
                              (plist-get object :body)))
          (t (dape--debug 'error
                          "Event ignored due to request seq %d < last handled seq %d"
                          seq dape--seq-event)))))
      (_ (dape--debug 'info "No handler for type %s" type)))))

(defun dape--process-filter (process string)
  "Filter for dape processes."
  (when (process-live-p process)
    (when-let ((input-buffer (process-buffer process))
               (buffer (current-buffer)))
      (with-current-buffer input-buffer
        (goto-char (point-max))
        (insert string)
        (goto-char (point-min))
        (let (done)
          (while (not done)
            (if-let ((object
                      (condition-case nil
                          (when (search-forward-regexp dape--content-length-re
                                                       nil
                                                       t)
                            (json-parse-buffer :object-type 'plist
                                               :null-object nil
                                               :false-object nil))
                        (error (and (erase-buffer) nil)))))
                (with-current-buffer buffer
                  (dape--handle-object process object))
              (setq done t))))
        (delete-region (point-min) (point))))))


;;; Outgoing requests

(defconst dape--timeout 5
  "Time before dape starts to complain about missing responses.")

(defun dape--create-timer (process seq)
  "Create SEQ request timeout timer for PROCESS."
  (puthash seq
           (run-with-timer dape--timeout
                           nil
                           (dape--callback
                            (dape--debug 'error
                                         "Timeout for reached for seq %d"
                                         seq)
                            (dape--update-state "timed out")
                            (remhash seq dape--timers)
                            (when-let ((cb (gethash seq dape--cb)))
                              (clrhash dape--tree-widget-open-p)
                              (remhash seq dape--cb)
                              (funcall cb process nil nil nil)))
                           process)
           dape--timers))

(defun dape-send-object (process seq object)
  "Helper for `dape-request' to send SEQ request with OBJECT to PROCESS."
  (let* ((object (plist-put object :seq seq))
         (json (json-serialize object :false-object nil))
         (string (format "Content-Length: %d\r\n\r\n%s" (length json) json)))
    (dape--debug 'io "Sending:\n%s" (pp-to-string object))
    (process-send-string process string)))

(defun dape-request (process command arguments &optional cb)
  "Send request COMMAND to PROCESS with ARGUMENTS.
If CB set, invoke CB on response.
See `dape--callback' for expected function signature."
  (let ((seq (setq dape--seq (1+ dape--seq)))
        (object (and arguments (list :arguments arguments))))
    (dape--create-timer process seq)
    (when cb
      (puthash seq cb dape--cb))
    (dape-send-object process
                      seq
                      (thread-first object
                                    (plist-put :type "request")
                                    (plist-put :command command)))))

(defun dape--initialize (process)
  "Send initialize request to PROCESS."
  (dape-request process
                "initialize"
                (list :clientID "dape"
                      :adapterID (plist-get dape--config
                                            :type)
                      :pathFormat "path"
                      :linesStartAt1 t
                      :columnsStartAt1 t
                      ;;:locale "en-US"
                      ;;:supportsVariableType t
                      ;;:supportsVariablePaging t
                      :supportsRunInTerminalRequest t
                      ;;:supportsMemoryReferences t
                      ;;:supportsInvalidatedEvent t
                      ;;:supportsMemoryEvent t
                      ;;:supportsArgsCanBeInterpretedByShell t
                      :supportsProgressReporting t
                      :supportsStartDebuggingRequest t
                      ;;:supportsVariableType t
                      )
                (dape--callback
                 (setq dape--capabilities body)
                 (when success
                   (dape--launch-or-attach process)))))

(defun dape--launch-or-attach (process)
  "Send launch or attach request to PROCESS.
Uses `dape--config' to derive type and to construct request."
  (if-let ((request (plist-get dape--config :request)))
      (dape-request process
                    request
                    (append
                     (cl-loop for (key value) on dape--config by 'cddr
                              when (keywordp key)
                              append (list key value))
                     (plist-get dape--config 'start-debugging))
                    (dape--callback
                     (when (plist-get dape--config 'start-debugging)
                       (plist-put dape--config 'start-debugging nil))))
    (user-error "Need to specify request in config `%S'"
                dape--config)
    (dape-kill)))

(defun dape--set-breakpoints (process buffer breakpoints &optional cb)
  "Set BREAKPOINTS in BUFFER by send setBreakpoints request to PROCESS.
BREAKPOINTS is an list of breakpoint overlays.
See `dape--callback' for expected CB signature."
  (let ((lines (mapcar (lambda (breakpoint)
                         (with-current-buffer (overlay-buffer breakpoint)
                           (line-number-at-pos (overlay-start breakpoint))))
                       breakpoints)))
    (dape-request process
                  "setBreakpoints"
                  (list
                   :source
                   (list
                    :name (file-name-nondirectory
                           (buffer-file-name buffer))
                    :path (buffer-file-name buffer))
                   :breakpoints
                   (cl-map
                    'vector
                    (lambda (overlay line)
                      (let (plist it)
                        (setq plist (list :line line))
                        (cond
                         ((setq it (overlay-get overlay 'dape-log-message))
                          (setq plist (plist-put plist :logMessage it)))
                         ((setq it (overlay-get overlay 'dape-expr-message))
                          (setq plist (plist-put plist :condition it))))
                        plist))
                    breakpoints
                    lines)
                   :lines (apply 'vector lines))
                  cb)))

(defun dape--set-main-breakpoints (process cb)
  "Set the main function breakpoints in adapter PROCESS.
The function names are derived from `dape-main-functions'.
See `dape--callback' for expected CB signature."
  (if (plist-get dape--capabilities :supportsFunctionBreakpoints)
      (dape-request process
                    "setFunctionBreakpoints"
                    (list
                     :breakpoints
                     (cl-map 'vector
                             (lambda (name)
                               (list :name name))
                             dape-main-functions))
                    cb)
    (funcall cb process)))

(defun dape--set-exception-breakpoints (process cb)
  "Set the exception breakpoints in adapter PROCESS.
The exceptions are derived from `dape--exceptions'.
See `dape--callback' for expected CB signature."
  (if dape--exceptions
      (dape-request process
                    "setExceptionBreakpoints"
                    (list
                     :filters
                     (cl-map 'vector
                             (lambda (exception)
                               (plist-get exception :filter))

                             (seq-filter (lambda (exception)
                                           (plist-get exception :enabled))
                                         dape--exceptions)))
                    cb)
    (funcall cb process)))

(defun dape--configure-exceptions (process cb)
  "Configure exception breakpoints in adapter PROCESS.
The exceptions are derived from `dape--exceptions'.
See `dape--callback' for expected CB signature."
  (setq dape--exceptions
        (cl-map 'list
                (lambda (exception)
                  (let ((stored-exception
                         (seq-find (lambda (stored-exception)
                                     (equal (plist-get exception :filter)
                                            (plist-get stored-exception :filter)))
                                   dape--exceptions)))
                    (cond
                     (stored-exception
                      (plist-put exception :enabled
                                 (plist-get stored-exception :enabled)))
                     ;; new exception
                     (t
                      (plist-put exception :enabled
                                 (plist-get exception :default))))))
                (plist-get dape--capabilities
                           :exceptionBreakpointFilters)))
  (dape--set-exception-breakpoints process
                                   (dape--callback
                                    (dape--info-update-widget dape--exceptions-widget)
                                    (funcall cb process))))

(defun dape--configure-breakpoints (process cb)
  "Configure breakpoints in adapter PROCESS.
See `dape--callback' for expected CB signature."
  (dape--clean-breakpoints)
  (if-let ((counter 0)
           (buffers-breakpoints (seq-group-by 'overlay-buffer
                                              dape--breakpoints)))
      (dolist (buffer-breakpoints buffers-breakpoints)
        (pcase-let ((`(,buffer . ,breakpoints) buffer-breakpoints))
          (dape--set-breakpoints process
                                 buffer
                                 breakpoints
                                 (dape--callback
                                  (setf counter (1+ counter))
                                  (when (eq counter (length buffers-breakpoints))
                                    (funcall cb process nil))))))
    (dape--set-main-breakpoints process cb)))

(defun dape--configuration-done (process)
  "End initialization of adapter PROCESS."
  (dape-request process
                "configurationDone"
                nil
                (dape--callback nil)))

(defun dape--get-threads (process stopped-id all-threads-stopped cb)
  "Helper for the stopped event to update `dape--threads'."
  (dape-request process
                "threads"
                nil
                (dape--callback
                 (setq dape--threads
                       (cl-map
                        'list
                        (lambda (new-thread)
                          (let ((thread
                                 (or (seq-find
                                      (lambda (old-thread)
                                        (eq (plist-get new-thread :id)
                                            (plist-get old-thread :id)))
                                      dape--threads)
                                     new-thread)))
                            (plist-put thread :name
                                       (plist-get new-thread :name))
                            (cond
                             (all-threads-stopped
                              (plist-put thread :status "stopped"))
                             ((eq (plist-get thread :id) stopped-id)
                              (plist-put thread :status "stopped"))
                             (t thread))))
                        (plist-get body :threads)))
                 (funcall cb process))))

(defun dape--stack-trace (process thread cb)
  "Update the stack trace in THREAD plist by adapter PROCESS.
See `dape--callback' for expected CB signature."
  (cond
   ((or (plist-get (dape--current-thread) :stackFrames)
        (not (integerp (plist-get thread :id))))
    (funcall cb process))
   (t
    (dape-request process
                  "stackTrace"
                  (list :threadId (plist-get thread :id)
                        :levels 50)
                  (dape--callback
                   (plist-put thread :stackFrames
                              (cl-map 'list
                                      'identity
                                      (plist-get body :stackFrames)))
                   (funcall cb process))))))

(defun dape--variables (process object cb)
  "Update OBJECTs variables by adapter PROCESS.
See `dape--callback' for expected CB signature."
  (let ((variables-reference (plist-get object :variablesReference)))
    (if (or (zerop variables-reference)
            (plist-get object :variables))
        (funcall cb process)
      (dape-request process
                    "variables"
                    (list :variablesReference variables-reference)
                    (dape--callback
                     (plist-put object
                                :variables
                                (thread-last (plist-get body :variables)
                                             (cl-map 'list 'identity)
                                             (seq-filter 'identity)))
                     (funcall cb process))))))

(defun dape--variables-recursive (process object path pred cb)
  "Update variables recursivly.
Get variable data from PROCESS and put result on OBJECT until PRED is nil.
PRED is called with PATH and OBJECT.
See `dape--callback' for expected CB signature."
  (let ((objects
         (seq-filter (apply-partially pred path)
                     (or (plist-get object :scopes)
                         (plist-get object :variables))))
        (requests 0))
    (if objects
        (dolist (object objects)
          (dape--with dape--variables (process object)
            (dape--with dape--variables-recursive (process
                                                   object
                                                   (cons (plist-get object :name)
                                                         path)
                                                   pred)
              (setq requests (1+ requests))
              (when (length= objects requests)
                (funcall cb process)))))
      (funcall cb process))))

(defun dape--evaluate-expression (process frame-id expression context cb)
  "Send evaluate request to PROCESS.
FRAME-ID specifies which frame the EXPRESSION is evaluated in and
CONTEXT which the result is going to be displayed in.
See `dape--callback' for expected CB signature."
  (dape-request process
                "evaluate"
                (list :frameId frame-id
                      :expression expression
                      :context context)
                cb))

(defun dape--scopes (process stack-frame cb)
  "Send scopes request to PROCESS for STACK-FRAME plist.
See `dape--callback' for expected CB signature."
  (if-let ((id (plist-get stack-frame :id))
           ((not (plist-get stack-frame :scopes))))
      (dape-request process
                    "scopes"
                    (list :frameId id)
                    (dape--callback
                     (let ((scopes (cl-map 'list 'identity (plist-get body :scopes))))
                       (plist-put stack-frame :scopes scopes)
                       (funcall cb process))))
    (funcall cb process)))

(defun dape--update (process &optional skip-clear-stack-frames)
  "Update dape data and ui.
PROCESS specifies adapter process.
If SKIP-CLEAR-STACK-FRAMES not all stack frame data is cleared.  This
is usefully if only to load data for another thread."
  (let ((current-thread (dape--current-thread)))
    (unless skip-clear-stack-frames
      (dolist (thread dape--threads)
        (plist-put thread :stackFrames nil)))
    (dolist (watched dape--watched)
      (plist-put watched :fetched nil))
    (dape--with dape--stack-trace (process current-thread)
      (dape--update-stack-pointers)
      (dape--with dape--scopes (process
                                (dape--current-stack-frame))
        (dape--with dape--variables-recursive (process
                                               (dape--current-stack-frame)
                                               nil
                                               (lambda (path _object)
                                                 (length< path 1)))
          (when dape-inline-variables
            (dape--update-inline-variables))
          (dape--update-widgets))))))


;;; Incoming requests

(cl-defgeneric dape-handle-request (_process command arguments)
  "Sink for all unsupported requests."
  (dape--debug 'info "Unhandled request '%S' with arguments %S"
               command
               arguments))

(cl-defmethod dape-handle-request (process (_command (eql runInTerminal)) arguments)
  "Handle runInTerminal requests.
Starts a new process to run process to be debugged."
  (let* ((cwd (plist-get process :cwd))
         (default-directory (or (and cwd
                                     (not (string-blank-p cwd))
                                     cwd)
                                default-directory))
         (buffer (get-buffer-create "*dape-shell*"))
         (display-buffer-alist
          '(((major-mode . shell-mode) . (display-buffer-no-window)))))
    (async-shell-command (string-join
                          (cl-map 'list
                                  'identity
                                  (plist-get arguments :args))
                          " ")
                         buffer
                         buffer)
    (display-buffer buffer '((display-buffer-in-side-window) .
                             '((side . bottom)
                               (slot . 1))))))

(cl-defmethod dape-handle-request (_process (_command (eql startDebugging)) arguments)
  "Handle startDebugging requests.
Starts a new process as per request of the debug adapter."
  (setq dape--parent-process dape--process)
  (dape dape--name
        (plist-put dape--config
                   'start-debugging
                   (plist-get arguments :configuration))))


;;; Events

(cl-defgeneric dape-handle-event (_process event body)
  "Sink for all unsupported events."
  (dape--debug 'info "Unhandled event '%S' with body %S" event body))

(cl-defmethod dape-handle-event (process (_event (eql initialized)) _body)
  "Handle initialized events."
  (dape--update-state "initialized")
  (dape--with dape--configure-exceptions (process)
    (dape--with dape--configure-breakpoints (process)
      (dape--configuration-done process))))

(cl-defmethod dape-handle-event (_process (_event (eql process)) body)
  "Handle process events."
  (let ((start-method (format "%sed"
                              (or (plist-get body :startMethod)
                                  "start"))))
    (dape--update-state start-method)
    (dape--repl-insert-text (format "Process %s %s\n"
                                    start-method
                                    (plist-get body :name)))))

(cl-defmethod dape-handle-event (_process (_event (eql thread)) body)
  "Handle thread events."
  (if-let ((thread
            (seq-find (lambda (thread)
                        (eq (plist-get thread :id)
                            (plist-get body :threadId)))
                      dape--threads)))
      (progn
        (plist-put thread :status (plist-get body :reason))
        (plist-put thread :name (or (plist-get thread :name)
                                    "unnamed")))
    ;; If new thread use thread state as global state
    (dape--update-state (plist-get body :reason))
    (push (list :status (plist-get body :reason)
                :id (plist-get body :threadId)
                :name "unnamed")
          dape--threads))
  (dape--info-update-threads-widget))

(cl-defmethod dape-handle-event (process (_event (eql stopped)) body)
  "Handle stopped events."
  (dape--update-state "stopped")
  (setq dape--thread-id (plist-get body :threadId))
  (dape--get-threads process
                     (plist-get body :threadId)
                     (plist-get body :allThreadsStopped)
                     (dape--callback
                      (dape--update process))))

(cl-defmethod dape-handle-event (_process (_event (eql continued)) body)
  "Handle continued events."
  (dape--remove-stack-pointers)
  (unless dape--thread-id
    (setq dape--thread-id (plist-get body :threadId)))
  (dape--update-state "running"))

(cl-defmethod dape-handle-event (_process (_event (eql output)) body)
  "Handle output events."
  (pcase (plist-get body :category)
    ("stdout"
     (dape--repl-insert-text (plist-get body :output)))
    ("stderr"
     (dape--repl-insert-text (plist-get body :output) 'error))
    ((or "console" "output")
     (dape--repl-insert-text (plist-get body :output) 'italic))))

(cl-defmethod dape-handle-event (_process (_event (eql exited)) body)
  "Handle exited events."
  (dape--update-state "exited")
  (dape--remove-stack-pointers)
  (dape--repl-insert-text (format "* Exit code: %d *\n"
                                  (plist-get body :exitCode))
                          (if (zerop (plist-get body :exitCode))
                              'dape-repl-exit-code-exit
                            'dape-repl-exit-code-fail)))

(cl-defmethod dape-handle-event (_process (_event (eql terminated)) _body)
  "Handle terminated events."
  (dape--update-state "terminated")
  (dape--repl-insert-text "* Program terminated *\n" 'italic)
  (dape--remove-stack-pointers))


;;; Startup/Setup

(defun dape--setup (process name config)
  "Helper for dape--start-* functions."
  (dape--remove-stack-pointers)
  (setq dape--name name
        dape--config config
        dape--seq 0
        dape--seq-event 0
        dape--timers (make-hash-table)
        dape--cb (make-hash-table)
        dape--thread-id nil
        dape--capabilities nil
        dape--threads nil
        dape--stack-id nil
        dape--process process)
  (setq dape--widget-guard nil
        dape--repl-insert-text-guard nil)
  (dape--update-state "starting")
  (run-hook-with-args 'dape-on-start-hooks)
  (dape--initialize process))

(defun dape--get-buffer ()
  "Setup and get *dape-processes* buffer."
  (let ((buffer (get-buffer-create "*dape-processes*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    buffer))

(defun dape--start-multi-session (name config)
  "Start multi session for NAME with CONFIG."
  (dape--debug 'info "Starting new multi session")
  (let ((buffer (dape--get-buffer))
        (default-directory (or (plist-get config 'command-cwd)
                               default-directory))
        (retries 30)
        process)
    (when (and (plist-get config 'command)
               (not (plist-get config 'start-debugging)))
      (setq dape--server-process
            (make-process :name "Dape server"
                          :command (cons (plist-get config 'command)
                                         (plist-get config 'command-args))
                          :buffer buffer
                          :sentinel 'dape--process-sentinel
                          :filter (lambda (_process string)
                                    (dape--debug 'server-stdout
                                                 "Server stdout:\n%s"
                                                 string))
                          :noquery t))
      (dape--debug 'info "Server process started %S"
                   (process-command dape--server-process)))
    (while (and (not process)
                (> retries 0))
      (ignore-errors
        (setq process
              (make-network-process :name (symbol-name name)
                                    :buffer buffer
                                    :host (plist-get config 'host)
                                    :service (plist-get config 'port)
                                    :sentinel 'dape--process-sentinel
                                    :filter 'dape--process-filter
                                    :noquery t)))
      (sleep-for 0 100)
      (setq retries (1- retries)))
    (if (zerop retries)
        (user-error "Unable to connect to server %s:%d"
                    (plist-get config 'host)
                    (plist-get config 'port))
      (dape--debug 'info "Connection to server established %s:%s"
                   (plist-get config 'host) (plist-get config 'port)))
    (dape--setup process name config)))

(defun dape--start-single-session (name config)
  "Start single session for NAME with CONFIG."
  (dape--debug 'info "Starting new single session")
  (let ((buffer (dape--get-buffer))
        (default-directory (or (plist-get config 'command-cwd)
                               default-directory))
        process)
    (setq process (make-process :name (symbol-name name)
                                :command (cons (plist-get config 'command)
                                               (plist-get config 'command-args))
                                :connection-type 'pipe
                                :coding 'no-conversion
                                :sentinel 'dape--process-sentinel
                                :filter 'dape--process-filter
                                :buffer buffer
                                :noquery t))
    (dape--debug 'info "Process started %S" (process-command process))
    (dape--setup process name config)))


;;; Commands

(defun dape-next (&optional arg)
  "Step one line (skip functions)."
  (interactive (list current-prefix-arg))
  (dape--next-like-command "next" arg))

(defun dape-step-in (&optional arg)
  "Steps into function/method. If not possible behaves like `dape-next'."
  (interactive (list current-prefix-arg))
  (dape--next-like-command "stepIn" arg))

(defun dape-step-out (&optional arg)
  "Steps out of function/method. If not possible behaves like `dape-next'."
  (interactive (list current-prefix-arg))
  (dape--next-like-command "stepOut" arg))

(defun dape-continue (&optional arg)
  "Resumes execution."
  (interactive (list current-prefix-arg))
  (dape--next-like-command "continue" arg))

(defun dape-pause ()
  "Pause execution."
  (interactive)
  (dape-request (dape--live-process) "pause" (dape--thread-id-object)))

(defun dape-restart ()
  "Restart last debug session started."
  (interactive)
  (cond
   ((and (dape--live-process t)
         (plist-get dape--capabilities :supportsRestartRequest))
    (setq dape--threads nil)
    (setq dape--thread-id nil)
    (dape-request dape--process "restart" nil))
   ((and dape--name dape--config)
    (dape dape--name dape--config))
   ((user-error "Unable to derive session to restart"))))

(defun dape-kill ()
  "Kill debug session."
  (interactive)
  (let* (done
         (kill-processes
          (lambda (&rest _)
            (ignore-errors
              (and dape--process
                   (delete-process dape--process))
              (and dape--server-process
                   (delete-process dape--server-process))
              (and dape--parent-process
                   (delete-process dape--parent-process)))
            (dape--remove-stack-pointers)
            (dape--variable-remove-overlays)
            ;; Clean mode-line after 2 seconds
            (run-with-timer 2 nil (lambda ()
                                    (unless (dape--live-process t)
                                      (setq dape--process nil)
                                      (force-mode-line-update t))))
            (setq done t))))
    (cond
     ((and (dape--live-process t)
           (plist-get dape--capabilities
                      :supportsTerminateRequest))
      (dape-request dape--process
                    "terminate"
                    nil
                    kill-processes)
      ;; Busy wait for response at least 2 seconds
      (cl-loop with max-iterations = 20
               for i from 1 to max-iterations
               until done
               do (accept-process-output nil 0.1)
               finally (unless done
                         (funcall kill-processes)
                         (dape--debug 'error
                                      "Terminate request timed out"))))
     ((and (dape--live-process t)
           (plist-get dape--capabilities
                      :supportTerminateDebuggee))
      (dape-request dape--process
                    "disconnect"
                    (list
                     :terminateDebuggee t)
                    kill-processes)
      ;; Busy wait for response at least 2 seconds
      (cl-loop with max-iterations = 20
               for i from 1 to max-iterations
               until done
               do (accept-process-output nil 0.1)
               finally (unless done
                         (funcall kill-processes)
                         (dape--debug 'error
                                      "Disconnect request timed out"))))
     (t
      (funcall kill-processes)))))

(defun dape-quit ()
  "Kill debug session and kill related dape buffers."
  (interactive)
  (dape-kill)
  (thread-last (buffer-list)
               (seq-filter (lambda (buffer)
                             (string-match-p "\\*dape-.+\\*" (buffer-name buffer))))
               (seq-do (lambda (buffer)
                         (when-let ((window (get-buffer-window buffer)))
                           (delete-window window))
                         (kill-buffer buffer)))))

(defun dape-toggle-breakpoint ()
  "Add or remove breakpoint at current line.
Will remove log or expression breakpoint at line added with
`dape-log-breakpoint' and/or `dape-expression-breakpoint'."
  (interactive)
  (if (dape--breakpoints-at-point '(dape-log-message dape-expr-message))
      (dape-remove-breakpoint-at-point '(dape-log-message dape-expr-message))
    (dape--place-breakpoint)))

(defun dape-log-breakpoint (log-message)
  "Add log breakpoint at line.
Argument LOG-MESSAGE contains string to print to *dape-repl*.
Expressions within `{}` are interpolated."
  (interactive
   (list
    (read-string "Log (Expressions within `{}` are interpolated): "
                 (when-let ((prev-log-breakpoint
                             (seq-find (lambda (ov)
                                         (overlay-get ov 'dape-log-message))
                                       (dape--breakpoints-at-point))))
                   (overlay-get prev-log-breakpoint 'dape-log-message)))))
  (when-let ((prev-log-breakpoint (seq-find (lambda (ov)
                                              (overlay-get ov 'dape-log-message))
                                            (dape--breakpoints-at-point))))
    (delq prev-log-breakpoint dape--breakpoints)
    (delete-overlay prev-log-breakpoint))
  (unless (string-empty-p log-message)
    (dape--place-breakpoint log-message)))

(defun dape-expression-breakpoint (expr-message)
  "Add expression breakpoint at current line.
When EXPR-MESSAGE is evaluated as true threads will pause at current line."
  (interactive
   (list
    (read-string "Condition: "
                 (when-let ((prev-expr-breakpoint
                             (seq-find (lambda (ov)
                                         (overlay-get ov 'dape-expr-message))
                                       (dape--breakpoints-at-point))))
                   (overlay-get prev-expr-breakpoint 'dape-expr-message)))))
  (when-let ((prev-expr-breakpoint
              (seq-find (lambda (ov)
                          (overlay-get ov 'dape-expr-message))
                        (dape--breakpoints-at-point))))
    (delq prev-expr-breakpoint dape--breakpoints)
    (delete-overlay prev-expr-breakpoint))
  (unless (string-empty-p expr-message)
    (dape--place-breakpoint nil expr-message)))

(defun dape-remove-breakpoint-at-point (&optional skip-types)
  "Remove breakpoint, log breakpoint and expression at current line.
SKIP-TYPES is a list of overlay properties to skip removal of."
  (interactive)
  (dolist (breakpoint (dape--breakpoints-at-point skip-types))
    (dape--remove-breakpoint breakpoint)))

(defun dape-remove-all-breakpoints ()
  "Remove all breakpoints."
  (interactive)
  (let ((buffers-breakpoints (seq-group-by 'overlay-buffer
                                           dape--breakpoints)))
    (dolist (buffer-breakpoints buffers-breakpoints)
      (pcase-let ((`(,buffer . ,breakpoints) buffer-breakpoints))
        (dolist (breakpoint breakpoints)
          (delq breakpoint dape--breakpoints)
          (delete-overlay breakpoint))
        (dape--update-breakpoints-in-buffer buffer))))
  (dape--info-update-breakpoints-widget))

(defun dape-select-thread (thread-id)
  "Selecte currrent thread by THREAD-ID."
  (interactive
   (list
    (let* ((collection
            (mapcar (lambda (thread) (cons (plist-get thread :name)
                                           (plist-get thread :id)))
                    dape--threads))
           (thread-name
            (completing-read (format "Select thread (current %s): "
                                     (plist-get (dape--current-thread) :name))
                             collection
                             nil t)))
      (alist-get thread-name collection nil nil 'equal))))
  (setq dape--thread-id thread-id)
  (dape--update (dape--live-process) t))

(defun dape-select-stack (stack-id)
  "Selected current stack by STACK-ID."
  (interactive
   (list
    (let* ((collection
            (mapcar (lambda (stack) (cons (plist-get stack :name)
                                          (plist-get stack :id)))
                    (thread-first (dape--current-thread)
                                  (plist-get :stackFrames))))
           (stack-name
            (completing-read (format "Select stack (current %s): "
                                     (plist-get (dape--current-stack-frame) :name))
                             collection
                             nil t)))
      (alist-get stack-name collection nil nil 'equal))))
  (setq dape--stack-id stack-id)
  (dape--update (dape--live-process) t))

(defun dape-watch-dwim (expression)
  "Add or remove watch for EXPRESSION.
Watched symbols are displayed in *dape-info* buffer.
*dape-info* buffer is displayed by executing the `dape-info' command."
  (interactive
   (list (string-trim
          (completing-read "Watch or unwatch symbol: "
                           (mapcar (lambda (plist) (plist-get plist :name))
                                   dape--watched)
                           nil
                           nil
                           (or (and (region-active-p)
                                    (buffer-substring (region-beginning)
                                                      (region-end)))
                               (thing-at-point 'symbol))))))
  (if-let ((plist
            (cl-find-if (lambda (plist)
                          (equal (plist-get plist :name)
                                 expression))
                        dape--watched)))
      (setq dape--watched
            (cl-remove plist dape--watched))
    (push (list :name expression)
          dape--watched))
  (dape--info-update-widget dape--watched-widget))

;;;###autoload
(defun dape (name options)
  "Start debugging session.

Start a debugging session based on NAME in `dape-configs' alist.
Entries in plist OPTIONS override config specified by NAME.
See `dape-configs' for more information on CONFIG.

When called as an interactive command, the first symbol like
string is read as NAME and rest as element in CONFIG.

Interactive example:
  launch :program \"bin\"

Executes launch `dape-configs' with :program as \"bin\"."
  (interactive (dape--read-config))
  (unless (plist-get options 'start-debugging)
    (dape-kill))
  (let ((base-config (alist-get name dape-configs))
        config)
    (unless base-config
      (user-error "Unable to find `%s' in `dape-configs'" name))
    (setq config
          (dape--config-eval
           (seq-reduce (apply-partially 'apply 'plist-put)
                       (seq-partition options 2)
                       (copy-tree base-config))))
    (when (called-interactively-p 'interactive)
      (push (dape--config-to-string name
                                    base-config
                                    config)
            dape--config-history))
    (unless (plist-get options 'start-debugging)
      (when-let ((buffer (get-buffer "*dape-debug*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))))
    (cond
     ((plist-get config 'compile)
      (dape--compile name config))
     ((and (plist-get config 'host) (plist-get config 'port))
      (dape--start-multi-session name config))
     (t
      (dape--start-single-session name config)))))


;;; Compile

(defun dape--compile-compilation-finish (buffer str)
  "Hook for `dape--compile-compilation-finish'.
Removes itself on execution."
  (remove-hook 'compilation-finish-functions #'dape--compile-compilation-finish)
  (cond
   ((equal "finished\n" str)
    (run-hook-with-args 'dape-compile-compile-hooks buffer)
    (dape dape--name (plist-put (copy-tree dape--config) 'compile nil)))
   (t
    (dape--repl-insert-text (format "* Compilation failed %s *" str)))))

(defun dape--compile (name config)
  "Start compilation for NAME and CONFIG."
  (let ((default-directory (plist-get config :cwd))
        (command (plist-get config 'compile)))
    (setq dape--config config)
    (setq dape--name name)
    (add-hook 'compilation-finish-functions #'dape--compile-compilation-finish)
    (funcall dape-compile-fn command)))


;;; Memory viewer

(defun dape--address-to-number (address)
  "Convert string ADDRESS to number."
  (if (string-match "\\`0x\\([[:alnum:]]+\\)" address)
      (string-to-number (match-string 1 address) 16)
    (string-to-number address)))

(defun dape-read-memory (memory-reference count)
  "Read COUNT bytes of memory at MEMORY-REFERENCE."
  (interactive
   (list (string-trim
          (read-string "Read memory reference: "
                       (when-let ((number (thing-at-point 'number)))
                         (number-to-string number))))
         (read-number "Count: " dape-read-memory-default-count)))
  (dape-request (dape--live-process)
                "readMemory"
                (list
                 :memoryReference memory-reference
                 :count count)
                (dape--callback
                 (when-let ((address (plist-get body :address))
                            (data (plist-get body :data)))
                   (setq address (dape--address-to-number address)
                         data (base64-decode-string data))
                   (let ((buffer (generate-new-buffer
                                  (format "*dape-memory @ %s*"
                                          memory-reference))))
                     (with-current-buffer buffer
                       (insert data)
                       (let (buffer-undo-list)
                         (hexl-mode))
                       ;; TODO Add hook with a writeMemory request
                       )
                     (pop-to-buffer buffer))))))


;;; Breakpoints

(defun dape--breakpoint-freeze (overlay _after _begin _end &optional _len)
  "Make sure that Dape OVERLAY region covers line."
  ;; FIXME Press evil "O" on a break point line this will mess things up
  (apply 'move-overlay overlay
         (dape--overlay-region (eq (overlay-get overlay 'category)
                                   'dape-stack-pointer))))

(defun dape--breakpoints-at-point (&optional skip-types)
  "Dape overlay breakpoints at point.
If SKIP-TYPES overlays with properties in SKIP-TYPES are filtered."
  (seq-filter (lambda (overlay)
                (and (eq 'dape-breakpoint (overlay-get overlay 'category))
                     (not (cl-some (lambda (skip-type)
                                     (overlay-get overlay skip-type))
                                   skip-types))))
              (overlays-in (line-beginning-position) (line-end-position))))

(defun dape--update-breakpoints-in-buffer (buffer)
  "Update all breakpoints in BUFFER."
  (when (buffer-live-p buffer)
    (when-let ((process (dape--live-process t))
               (breakpoints (thread-last dape--breakpoints
                                         (seq-group-by 'overlay-buffer)
                                         (alist-get buffer))))
      (dape--set-breakpoints process buffer breakpoints))))

(defun dape--place-breakpoint (&optional log-message expression)
  "Place breakpoint at current line.
If LOG-MESSAGE place log breakpoint.
If EXPRESSION place conditional breakpoint."
  (unless (derived-mode-p 'prog-mode)
    (user-error "Trying to set breakpoint in none `prog-mode' buffer"))
  (let ((breakpoint (apply 'make-overlay (dape--overlay-region))))
    (overlay-put breakpoint 'window t)
    (overlay-put breakpoint 'category 'dape-breakpoint)
    (cond
     (log-message
      (overlay-put breakpoint 'dape-log-message log-message)
      (overlay-put breakpoint 'after-string (concat
                                             " "
                                             (propertize
                                              (format "Log: %s" log-message)
                                              'face 'dape-log-face))))
     (expression
      (overlay-put breakpoint 'dape-expr-message expression)
      (overlay-put breakpoint 'after-string (concat
                                             " "
                                             (propertize
                                              (format "Break: %s" expression)
                                              'face 'dape-expression-face))))
     (t
      (overlay-put breakpoint 'before-string
                   (propertize "B"
                               'face 'dape-breakpoint-face))))
    (overlay-put breakpoint 'modification-hooks '(dape--breakpoint-freeze))
    (push breakpoint dape--breakpoints))
  (dape--info-update-breakpoints-widget)
  (dape--update-breakpoints-in-buffer (current-buffer)))

(defun dape--remove-breakpoint (overlay)
  "Remove OVERLAY breakpoint from buffer and session."
  (delq overlay dape--breakpoints)
  (dape--update-breakpoints-in-buffer (overlay-buffer overlay))
  (delete-overlay overlay)
  (dape--info-update-breakpoints-widget))

(defun dape--clean-breakpoints ()
  "Clean breakpoint list of all overlays that does not have a buffer."
  (setq dape--breakpoints (seq-filter 'overlay-buffer
                                      dape--breakpoints)))


;;; Stack pointers

(defun dape--place-stack-pointer (marker &optional face prefix)
  "Place stack trace overlay at MARKER.
Use FACE to style overlay.
If PREFIX is non nil add PREFIX to stack pointer."
  (when marker
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let ((stack-pointer
               (apply 'make-overlay (dape--overlay-region t))))
          (overlay-put stack-pointer 'face face)
          (overlay-put stack-pointer 'priority 1)
          (overlay-put stack-pointer 'window t)
          (overlay-put stack-pointer 'category 'dape-stack-pointer)
          (overlay-put stack-pointer 'before-string (concat prefix "→"))
          (overlay-put stack-pointer 'modification-hooks
                       '(dape--breakpoint-freeze))
          stack-pointer)))))

(defun dape--remove-stack-pointers ()
  "Remove stack pointer overlays."
  (dolist (overlay dape--stack-pointers)
    (when-let ((buffer (overlay-buffer overlay)))
      (with-current-buffer buffer
        (dape--remove-eldoc-hook)))
    (delete-overlay overlay))
  (setq dape--stack-pointers nil))

(defun dape--place-stack-pointers (thread)
  "Place stack trace pointers for THREAD."
  (when-let ((stopped-event-thread-p (eq dape--thread-id
                                         (plist-get thread :id)))
             (current-stack-frame (dape--current-stack-frame))
             (index 0))
    (dolist (stack-frame (thread-first (dape--current-thread)
                                       (plist-get :stackFrames)))
      (let ((selected (eq current-stack-frame stack-frame)))
        (when-let* ((marker (dape--object-to-marker stack-frame
                                                    (unless selected
                                                      'get-file-buffer)))
                    (overlay (dape--place-stack-pointer marker
                                                        (when selected
                                                          'dape-stack-trace)
                                                        (unless (zerop index)
                                                          (number-to-string index)))))
          (when-let ((buffer (overlay-buffer overlay)))
            (with-current-buffer buffer
              (dape--add-eldoc-hook)))
          (when selected
            (dape--goto-source stack-frame
                               (memq major-mode
                                     '(dape-repl-mode dape-info-mode))))
          (push overlay
                dape--stack-pointers))
        (setq index (1+ index))))))


;;; Variable overlays

(defun dape--variable-re-refs (str beg end)
  "Naively find references to variable like STR in current buffer.
Search is bounded to BEG and END."
  (let ((regex (format "\\_<%s\\_>" (regexp-quote str)))
        (case-fold-search nil)
        refs)
    (goto-char beg)
    (save-match-data
      (while (re-search-forward regex end t)
        (let ((face (get-text-property 0 'face (match-string 0))))
          (when (or (eq face 'font-lock-variable-name-face)
                    (not face))
            (push (point-marker)
                  refs)))))
    refs))

(defun dape--variable-add-overlay (variable marker)
  "Add inline variable overlay for VARIABLE at MARKER."
  (when-let ((buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (when-let* ((ov (make-overlay (1- (marker-position marker)) marker))
                  (var-string (plist-get variable :value))
                  (max-length (or (string-match-p "\n" var-string)
                                  dape-inline-variable-length))
                  (ov-string (concat (string-limit var-string max-length)
                                     (when (length> var-string max-length)
                                       (propertize "..." 'face 'shadow)))))
        (overlay-put ov 'after-string
                     (format " %s " (propertize ov-string 'face 'shadow)))
        (overlay-put ov 'evaporate t)
        (push ov dape--variable-overlays)))))

(defun dape--create-scope-overlays (scope)
  "Add overlays for SCOPE in selected buffer."
  (when-let ((buffer (or (and-let* ((path (thread-first scope
                                                        (plist-get :source)
                                                        (plist-get :path)))
                                    (buffer (find-file-noselect path)))
                           buffer)
                         (and-let* ((path (thread-first (dape--current-stack-frame)
                                                        (plist-get :source)
                                                        (plist-get :path)))
                                    (buffer (find-file-noselect path)))
                           buffer))))
    (with-current-buffer buffer
      (save-excursion
        (let ((beg (or (when-let ((line (plist-get scope :line)))
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- line))
                           (point)))
                       (point-min)))
              (end (or (when-let ((line (plist-get scope :endLine)))
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- line))
                           (point)))
                       (point-max))))
          (dolist (variable (plist-get scope :variables))
            (dolist (marker (dape--variable-re-refs (plist-get variable :name)
                                                    beg
                                                    end))
              (dape--variable-add-overlay variable marker))))))))

(defun dape--variable-remove-overlays ()
  "Remove all variable overlays."
  (dolist (ov dape--variable-overlays)
    (delete-overlay ov)))


;;; Info buffer

(define-widget 'dape--tree-widget-open 'tree-widget-open-icon
  "Icon for an expanded dape--tree-widget node."
  :tag "-")

(define-widget 'dape--tree-widget-close 'tree-widget-close-icon
  "Icon for a collapsed dape--tree-widget node."
  :tag "+")

(define-widget 'dape--tree-widget-empty 'tree-widget-empty-icon
  "Icon for an expanded dape--tree-widget node with no child."
  :tag "X")

(define-widget 'dape--tree-widget-leaf 'tree-widget-leaf-icon
  "Icon for a dape--tree-widget node with no child."
  :tag "•")

(define-widget 'dape--tree-widget-space 'item
  "Icon for all dape--tree-widget guides."
  :format " ")

(defun dape--tree-widget-action (tree &optional event)
  "Handle the :action of TREE with EVENT.
Stores :open state in `dape--tree-widget-open-p'."
  (tree-widget-action tree event)
  ;; Cache current keystate
  (puthash (widget-get tree :path)
           (widget-get tree :open)
           dape--tree-widget-open-p)
  ;; If expanded set all parent to be expanded as well
  (when (widget-get tree :open)
    (let ((parent (widget-get tree :parent)))
      (while parent
        (puthash (widget-get parent :path)
                 t
                 dape--tree-widget-open-p)
        (setq parent (widget-get parent :parent))))))

(defun dape--tree-widget-convert-widget (tree)
  "Convert the TREE open state from `dape--tree-widget-open-p'."
  (widget-put tree :path
              (cons (widget-get tree :key)
                    (widget-get (widget-get tree :parent) :path)))
  (widget-put tree :open
              (gethash (widget-get tree :path)
                       dape--tree-widget-open-p
                       (widget-get tree :default)))
  (tree-widget-convert-widget tree))

(define-widget 'dape--tree-widget 'tree-widget
  "Widget based on tree-widget but with :open cache."
  :convert-widget 'dape--tree-widget-convert-widget
  :default        nil
  :key            nil
  :path           nil
  :action         'dape--tree-widget-action
  :open-icon      'dape--tree-widget-open
  :close-icon     'dape--tree-widget-close
  :empty-icon     'dape--tree-widget-empty
  :leaf-icon      'dape--tree-widget-leaf
  :guide          'dape--tree-widget-space
  :no-guide       'dape--tree-widget-space
  :end-guide      'dape--tree-widget-space
  :nohandle-guide 'dape--tree-widget-space
  :handle         'dape--tree-widget-space
  :no-handle      'dape--tree-widget-space)

(defun dape--widget-sanitize-string (string)
  "Sanitize STRING for widget usage."
  (save-match-data
    (replace-regexp-in-string "%" "%%" string)))

(defmacro dape--with-update-ui-guard (fn args &rest body)
  "Guard BODY from being run in the middle of widget updates.
If BODY is invoked in the middle of widget updates, invoke FN with
ARGS after 1 second."
  (declare (indent 2))
  `(cond
    (dape--widget-guard
     ;; TODO figure out the performance impact
     (run-with-timer 1 nil ,fn ,@args))
    (t
     (setq dape--widget-guard t)
     (ignore-errors ,@body)
     (setq dape--widget-guard nil))))

(defun dape--info-update-threads-widget ()
  "Update threads widget in *dape-info* buffer."
  (dape--info-update-widget dape--threads-widget))

(defun dape--info-update-breakpoints-widget ()
  "Update breakpoints widget in *dape-info* buffer."
  (dape--info-update-widget dape--breakpoints-widget))

(defun dape--info-update-widget (&rest widgets)
  "Update WIDGETS in *dape-info* buffer."
  (dape--with-update-ui-guard 'dape--info-update-widget (widgets)
    (when-let ((buffer (get-buffer "*dape-info*")))
      ;; FIX this seams owerkill, should be a cleaner way
      (with-current-buffer buffer
        (with-selected-window (get-buffer-window)
          (save-window-excursion
            (dolist (widget widgets)
              (widget-value-set widget
                                (widget-value-value-get widget)))))))))

(defconst dape--info-variables-fetch-depth 4
  "Depth of variables to fetch on stopped event.")

(defun dape--info-update-scope-widget ()
  "Update variable widget.
Depth is decided by `dape--info-variables-fetch-depth'."
  (dape--with dape--scopes ((dape--live-process) (dape--current-stack-frame))
    (dape--with dape--variables-recursive (process
                                           (dape--current-stack-frame)
                                           '("Variables")
                                           (lambda (path object)
                                             (and (length< path dape--info-variables-fetch-depth)
                                                  (gethash (cons (plist-get object :name) path)
                                                           dape--tree-widget-open-p))))
      (dape--info-update-widget dape--scopes-widget))))

(defun dape--expand-threads (_tree)
  "Expander for `dape--threads-widget' widget."
  (mapcar (lambda (thread)
            (widget-convert 'file-link
                            :id (plist-get thread :id)
                            :format (concat
                                     (if (eq (plist-get thread :id)
                                             dape--thread-id)
                                         "→ %t"
                                       "%[%t%]")
                                     (format " [%s]"
                                             (propertize
                                              (plist-get thread :status)
                                              'face 'bold))
                                     "\n")
                            :action (lambda (widget &rest _)
                                      (dape-select-thread (widget-get widget :id)))
                            :tag (plist-get thread :name)))
          dape--threads))

(defun dape--expand-stack-p (tree)
  "Expander predicate for `dape--threads-widget' widget."
  (cond
   ((plist-get (dape--current-thread) :stackFrames)
    t)
   ((not (equal (plist-get (dape--current-thread) :status)
                "stopped"))
    nil)
   (t
    (dape--stack-trace (dape--live-process)
                       (dape--current-thread)
                       (dape--callback
                        (when (plist-get (dape--current-thread) :stackFrames)
                          (dape--info-update-widget tree))))
    nil)))

(defun dape--expand-stack (_tree)
  "Expander for `dape--stack-widget' widget."
  (let ((current-thread (dape--current-thread))
        (current-stack-frame (dape--current-stack-frame)))
    (when (equal (plist-get current-thread :status) "stopped")
      (mapcar (lambda (stack-frame)
                (widget-convert
                 'file-link
                 :id (plist-get stack-frame :id)
                 :format (concat
                          (if (eq (plist-get current-stack-frame :id)
                                    (plist-get stack-frame :id))
                              "→ "
                            "")
                          (if-let ((path (thread-first stack-frame
                                                       (plist-get :source)
                                                       (plist-get :path))))
                              (format "%%t %%[%s%%]\n"
                                      (dape--widget-sanitize-string
                                       (dape--format-file-line path
                                                               (plist-get stack-frame
                                                                          :line))))
                            "%t\n"))
                 :action (lambda (widget &rest _)
                           (dape-select-stack (widget-get widget :id)))
                 :tag (propertize (plist-get stack-frame :name)
                                  'face 'font-lock-function-name-face)))
              (plist-get current-thread :stackFrames)))))

(defun dape--variable-to-widget (tree variable)
  "Create variable widget from VARIABLE under TREE."
  (cond
   ((zerop (plist-get variable :variablesReference))
    (widget-convert
     'item
     :tag (dape--variable-string variable)))
   (t
    (widget-convert
     'dape--tree-widget
     :parent tree
     :key (plist-get variable :name)
     :default (equal (plist-get variable :presentationHint) "locals")
     :tag (dape--variable-string variable)
     :expander-p
     (lambda (tree)
       (if (plist-get variable :variables)
           t
         (dape--variables (dape--live-process)
                          variable
                          (dape--callback
                           (when (plist-get variable :variables)
                             (dape--info-update-widget tree))))
         nil))
     :expander
     (lambda (tree)
       (mapcar (apply-partially 'dape--variable-to-widget tree)
               (plist-get variable :variables)))))))

(defun dape--expand-scopes-p (tree)
  "Expander predicate for `dape--scopes-widget'."
  (cond
   ((not (equal (plist-get (dape--current-thread) :status) "stopped"))
    nil)
   ((plist-get (dape--current-stack-frame) :scopes)
    t)
   (t
    (dape--scopes (dape--live-process)
                  (dape--current-stack-frame)
                  (dape--callback
                   (when (plist-get (dape--current-stack-frame) :scopes)
                     (dape--info-update-widget tree))))
    nil)))

(defun dape--expand-scopes (tree)
  "Expander predicate for `dape--scopes-widget'."
  (mapcar (apply-partially 'dape--variable-to-widget tree)
          (plist-get (dape--current-stack-frame) :scopes)))

(defun dape--expand-watched-p (tree)
  "Expander predicate for `dape--watched-widget'."
  (cond
   ((not (equal (plist-get (dape--current-thread) :status) "stopped"))
    nil)
   ((not (cl-every (lambda (plist)
                     (plist-get plist :fetched))
                   dape--watched))
    (funcall
     (cl-reduce (lambda (cb plist)
                  (dape--callback
                   (dape--evaluate-expression
                    (dape--live-process)
                    (plist-get (dape--current-stack-frame) :id)
                    (plist-get plist :name)
                    "watch"
                    (dape--callback
                     (when success
                       (cl-loop for (key value) on body by 'cddr
                                do (plist-put plist key value)))
                     (plist-put plist :fetched t)
                     (funcall cb process)))))
                dape--watched
                :initial-value
                (dape--callback
                 (dape--info-update-widget tree)))
     (dape--live-process))
    t)
   (t t)))

(defun dape--expand-watched (tree)
  "Expander for `dape--watched-widget'."
  (thread-last dape--watched
               (mapcar (lambda (plist)
                         (if (plist-get plist :result)
                             (dape--variable-to-widget tree plist)
                           (widget-convert 'item
                                           :value (dape--variable-string plist)))))))

(defun dape--expand-breakpoints-widget (_tree)
  "Expander for `dape--breakpoints-widget'."
  (let ((current-stopped-files-lines
         (thread-last (dape--stopped-threads)
                      (mapcan
                       (lambda (thread)
                         (when-let* ((stack-frame
                                      (car (plist-get thread
                                                      :stackFrames)))
                                     (file
                                      (thread-first stack-frame
                                                    (plist-get :source)
                                                    (plist-get :path)))
                                     (line
                                      (plist-get stack-frame :line)))
                           (list (cons file line))))))))
    (mapcan (lambda (overlay)
              (when-let* ((buffer (overlay-buffer overlay))
                          (file (buffer-file-name buffer))
                          (line
                           (with-current-buffer buffer
                             (line-number-at-pos (overlay-start overlay)))))
                (list
                 (widget-convert
                  'file-link
                  :format (concat
                           "%t%[%v%]"
                           ;; % needs to be escaped for widget-format but
                           ;; this is not without issue as widget-format
                           ;; inserts the escaped % without 'face.
                           (when-let ((after-string
                                       (overlay-get overlay
                                                    'after-string)))
                             (dape--widget-sanitize-string after-string))
                           "\n")
                  :action (lambda (&rest _)
                            (dape--goto-source `(:source (:path ,file)
                                                         :line ,line)
                                               nil
                                               t))
                  :tag (if (member (cons file line)
                                   current-stopped-files-lines)
                           (propertize "→ " 'face 'bold)
                         "")
                  :value (dape--format-file-line file line)))))
            dape--breakpoints)))

(defun dape--expand-exceptions-widget (_)
  "Expander for `dape--exceptions-widget'."
  (mapcar (lambda (exception)
            (widget-convert
             'toggle
             :format (format "%s %%[%%v%%]\n"
                             (plist-get exception :label))
             :value (plist-get exception :enabled)
             :action (lambda (&rest _args)
                       ;; HACK updates exceptions tree after enabling exception
                       ;;      this is only only done to get the current
                       ;;      exception object.
                       (plist-put exception :enabled
                                  (not (plist-get exception :enabled)))
                       (dape--set-exception-breakpoints
                        (dape--live-process)
                        (dape--callback
                         (dape--info-update-widget dape--exceptions-widget))))))
          dape--exceptions))

(defun dape--info-press-widget-at-line (predicate)
  "Press first widget on current line matching PREDICATE."
  (save-excursion
    (if (funcall predicate (widget-at))
        (widget-button-press (point))
      (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'line))
                  (found))
        (goto-char start)
        (while (and (not found)
                    (< (point) end))
          (cond
           ((funcall predicate (widget-at))
            (widget-button-press (point))
            (setq found t))
           ((eobp)
            (setq found t))
           (t
            (goto-char (next-overlay-change (point))))))))))

(defun dape-info-buton-press-dwim ()
  "Press button, change stack, change thread or goto breakpoint.
Depending on line in *dape-info* buffer."
  (interactive)
  (dape--info-press-widget-at-line
   (lambda (widget)
     (memq (widget-type widget)
           '(file-link link toggle)))))

(defun dape-info-tree-dwim ()
  "Toggle tree expansion in *dape-info* buffer."
  (interactive)
  (dape--info-press-widget-at-line
   (lambda (widget)
     (memq (widget-type widget)
           '(dape--tree-widget-open dape--tree-widget-close)))))

(defvar dape-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'dape-info-buton-press-dwim)
    (define-key map (kbd "<tab>") 'dape-info-tree-dwim)
    map)
  "Keymap active in *dape-info* buffer.")

(define-derived-mode dape-info-mode special-mode "Dape info"
  "Dape info mode is displays various dape related information.
See `dape-info' for more information."
  :group 'dape
  :interactive nil
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq-local buffer-read-only         t
              truncate-lines           t
              indent-tabs-mode         nil
              desktop-save-buffer      nil
              tree-widget-image-enable nil))

(defun dape-info (&optional select-buffer)
  "Create or select *dape-info* buffer.
Buffer contains debug session information.  Select the buffer if invoked
interactively or if SELECT-BUFFER is non nil."
  (interactive (list t))
  (let ((buffer (get-buffer-create "*dape-info*"))
        window)
    (with-current-buffer buffer
      (dape-info-mode)
      (dolist (button dape-info-buttons)
        (pcase-let ((`(,name . ,fn) button))
          (widget-create 'link
                         :value name
                         :action
                         (lambda (&rest _)
                           (funcall fn)))
          (widget-insert " ")))
      (when dape-info-buttons
        (widget-insert "\n")
        (forward-line -1)
        (let ((ov (make-overlay (line-beginning-position)
                                (line-beginning-position 2))))
          (overlay-put ov 'face 'region)
          (overlay-put ov 'priority 1))
        (forward-line))
      (setq dape--watched-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Watched" 'face 'bold)
                           :path '("Watched")
                           :open t
                           :expander-p 'dape--expand-watched-p
                           :expander 'dape--expand-watched)
            dape--scopes-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Variables" 'face 'bold)
                           :path '("Variables")
                           :open t
                           :expander-p 'dape--expand-scopes-p
                           :expander 'dape--expand-scopes)
            dape--stack-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Stack" 'face 'bold)
                           :path '("Stack")
                           :open t
                           :expander-p 'dape--expand-stack-p
                           :expander 'dape--expand-stack)
            dape--threads-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Threads" 'face 'bold)
                           :path '("Threads")
                           :open t
                           :expander-p 'identity ;; Always expand
                           :expander 'dape--expand-threads)
            dape--breakpoints-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Breakpoints" 'face 'bold)
                           :path '("Breakpoints")
                           :open t
                           :expander-p 'identity ;; Always expand
                           :expander 'dape--expand-breakpoints-widget)
            dape--exceptions-widget
            (widget-create 'dape--tree-widget
                           :tag (propertize "Exceptions" 'face 'bold)
                           :path '("Exceptions")
                           :open t
                           :expander-p 'identity ;; Always expand
                           :expander 'dape--expand-exceptions-widget))
      (widget-setup))
    (setq window (display-buffer buffer
                                 '((display-buffer-in-side-window)
                                   . ((side . left)))))
    (when select-buffer
      (select-window window)
      (goto-char (point-min)))))


;;; REPL buffer

(defun dape--repl-insert-text (msg &optional face)
  "Insert MSG with FACE in *dape-repl* buffer."
  (cond
   (dape--repl-insert-text-guard
    (run-with-timer 0.1 nil 'dape--repl-insert-text msg))
   (t
    (setq dape--repl-insert-text-guard t)
    (when-let ((buffer (get-buffer "*dape-repl*")))
      (with-current-buffer buffer
        (save-excursion
          (condition-case err
              (progn
                (goto-char (point-max))
                (comint-previous-prompt 0)
                (forward-line -1)
                (end-of-line)
                (when-let (line (thing-at-point 'line))
                  (when (eq (aref line 0) ?>)
                    (let ((inhibit-read-only t))
                      (insert "\n"))))
                (let ((inhibit-read-only t))
                  (insert (propertize msg 'font-lock-face face))))
            (error
             (setq dape--repl-insert-text-guard nil)
             (signal (car err) (cdr err))))
          (setq dape--repl-insert-text-guard nil))))
    (unless (get-buffer-window "*dape-repl*")
      (when (stringp msg)
        (message (format "%s"
                         (string-trim msg "\\\n" "\\\n"))
                 'face face))))))

(defun dape--repl-input-sender (dummy-process input)
  "Dape repl `comint-input-sender'."
  (let (cmd)
    (if (not (dape--live-process t))
        (comint-output-filter dummy-process
                              "DAPE server not running\n")
      (cond
       ;; Run previous input
       ((and (string-empty-p input)
             (not (string-empty-p (car (ring-elements comint-input-ring)))))
        (when-let ((last (car (ring-elements comint-input-ring))))
          (dape--repl-input-sender dummy-process last)))
       ;; Run command from `dape-named-commands'
       ((setq cmd
              (or (alist-get input dape-repl-commands nil nil 'equal)
                  (and dape-repl-use-shorthand
                       (cl-loop for (key . value) in dape-repl-commands
                                when (equal (substring key 0 1) input)
                                return value))))
        (setq dape--repl-insert-text-guard t)
        (comint-output-filter dummy-process "\n> ")
        (funcall cmd)
        (setq dape--repl-insert-text-guard nil))
       ;; Evaluate expression
       ((dape--stopped-threads)
        ;; FIXME `dape--repl-insert-text-guard' is used here to not mess up ordering
        ;;       when running commands that will itself trigger output request
        (setq dape--repl-insert-text-guard t)
        (dape--evaluate-expression (dape--live-process)
                                   (plist-get (dape--current-stack-frame) :id)
                                   (substring-no-properties input)
                                   "repl"
                                   (dape--callback
                                    (comint-output-filter dummy-process
                                                          (concat
                                                           (if success
                                                               (plist-get body :result)
                                                             msg)
                                                           "\n\n> "))
          (setq dape--repl-insert-text-guard nil))))
       (t
        (comint-output-filter
         dummy-process
         (format "* Unable to send \"%s\" no stopped threads *\n> "
                 input)))))))

(defun dape--repl-completion-at-point ()
  "Completion at point function for *dape-repl* buffer."
  ;; FIXME repl completion needs some work
  (let* ((bounds (save-excursion
                   (cons (and (skip-chars-backward "^\s")
                              (point))
                         (and (skip-chars-forward "^\s")
                              (point)))))
         (column (1+ (- (cdr bounds) (car bounds))))
         (str (buffer-substring-no-properties
               (car bounds)
               (cdr bounds)))
         (collection
          (mapcar (lambda (cmd)
                    (cons (car cmd)
                          (format " %s"
                                  (propertize (symbol-name (cdr cmd))
                                              'face 'font-lock-builtin-face))))
                  dape-repl-commands))
         done)
    (list
     (car bounds)
     (cdr bounds)
     (completion-table-dynamic
      (lambda (_str)
        (when-let ((frame-id (plist-get (dape--current-stack-frame) :id)))
          (dape-request
           (dape--live-process)
           "completions"
           (list :frameId frame-id
                 :text str
                 :column column
                 :line 1)
           (dape--callback
            (setq collection
                  (append
                   collection
                   (mapcar
                    (lambda (target)
                      (cons
                       (cond
                        ((plist-get target :text)
                         (plist-get target :text))
                        ((and (plist-get target :label)
                              (plist-get target :start))
                         (let ((label (plist-get target :label))
                               (start (plist-get target :start)))
                           (concat (substring str 0 start)
                                   label
                                   (substring str
                                              (thread-first
                                                target
                                                (plist-get :length)
                                                (+ 1 start)
                                                (min (length str)))))))
                        ((and (plist-get target :label)
                              (memq (aref str (1- (length str))) '(?. ?/ ?:)))
                         (concat str (plist-get target :label)))
                        ((and (plist-get target :label)
                              (length> (plist-get target :label)
                                       (length str)))
                         (plist-get target :label))
                        ((and (plist-get target :label)
                              (length> (plist-get target :label)
                                       (length str)))
                         (cl-loop with label = (plist-get target :label)
                                  for i downfrom (1- (length label)) downto 1
                                  when (equal (substring str (- (length str) i))
                                              (substring label 0 i))
                                  return (concat str (substring label i))
                                  finally return label)))
                       (when-let ((type (plist-get target :type)))
                         (format " %s"
                                 (propertize type
                                             'face 'font-lock-type-face)))))
                    (plist-get body :targets))))
            (setq done t)))
          (while-no-input
            (while (not done)
              (accept-process-output nil 0 1)))
          collection)))
     :annotation-function
     (lambda (str)
       (when-let ((annotation
                   (alist-get (substring-no-properties str) collection
                              nil nil 'equal)))
         annotation)))))


(defvar dape--repl--prompt "> "
  "Dape repl prompt.")
(defvar dape-repl-mode nil)

(define-derived-mode dape-repl-mode comint-mode "Dape REPL"
  "Mode for *dape-repl* buffer."
  :group 'dape
  :interactive nil
  (when dape-repl-mode
    (user-error "`dape-repl-mode' all ready enabled"))
  (setq-local dape-repl-mode t
              comint-prompt-read-only t
              comint-input-sender 'dape--repl-input-sender
              comint-prompt-regexp (concat "^" (regexp-quote dape--repl--prompt))
              comint-process-echoes nil)
  (add-hook 'completion-at-point-functions #'dape--repl-completion-at-point nil t)
  ;; Stolen from ielm
  ;; Start a dummy process just to please comint
  (unless (comint-check-proc (current-buffer))
    (start-process "dape-repl" (current-buffer) nil)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer))
                                    nil)
    (set-process-filter (get-buffer-process (current-buffer))
                        'comint-output-filter)
    (insert (propertize
               (format
                "* Welcome to Dape REPL! *
Available Dape commands: %s
Empty input will rerun last command.\n\n\n"
                (mapconcat 'identity
                           (mapcar (lambda (cmd)
                                     (let ((str (car cmd)))
                                       (if dape-repl-use-shorthand
                                           (concat "["
                                                   (substring str 0 1)
                                                   "]"
                                                   (substring str 1))
                                         str)))
                                   dape-repl-commands)
                           ", "))
               'font-lock-face 'italic))
    (set-marker (process-mark (get-buffer-process (current-buffer))) (point))
    (comint-output-filter (get-buffer-process (current-buffer))
                          dape--repl--prompt)))

(defun dape-repl ()
  "Create or select *dape-repl* buffer."
  (interactive)
  (let ((buffer-name "*dape-repl*")
        window)
    (with-current-buffer (get-buffer-create buffer-name)
      (unless dape-repl-mode
        (dape-repl-mode))
      (setq window (display-buffer (current-buffer)
                                   '((display-buffer-reuse-window
                                      display-buffer-in-side-window)
                                     . ((side . bottom)
                                        (slot . -1)))))
      (when (called-interactively-p 'interactive)
        (select-window window)))))


;;; Config

(defvar dape-history nil
  "History variable for `dape'.")

(defun dape--config-eval-value (value &optional skip-function)
  "Evaluate dape config VALUE.
If SKIP-FUNCTION and VALUE is an function it is not invoked."
  (cond
   ((functionp value) (or (and skip-function value)
                          (funcall-interactively value)))
   ((plistp value) (dape--config-eval value skip-function))
   ((vectorp value) (cl-map 'vector
                            (lambda (v)
                              (dape--config-eval-value v skip-function))
                            value))
   ((and (symbolp value)
         (not (eq (symbol-value value) value)))
    (dape--config-eval-value (symbol-value value) skip-function))
   (t value)))

(defun dape--config-eval (config &optional skip-functions)
  "Evaluate CONFIG.
If SKIP-FUNCTIONS function values are not called during evaluation."
  (cl-loop for (key value) on config by 'cddr
           append (cond
                   ((memq key '(modes)) (list key value))
                   (t (list key (dape--config-eval-value value
                                                         skip-functions))))))

(defun dape--config-from-string (str)
  "Parse list of name and config from STR."
  (let (name read-config base-config)
    (when (string-empty-p str)
      (user-error "Expected config name"))
    (setq name (read str)
          base-config (copy-tree (alist-get name dape-configs))
          str (substring str (length (symbol-name name))))
    (unless (string-empty-p str)
      (setq read-config (read (format "(%s)" str))))
    (unless (plistp read-config)
      (user-error "Unexpected config format, see `dape-configs'"))
    (cl-loop for (key value) on read-config by 'cddr
             do (setq base-config (plist-put base-config key value)))
    (list name base-config)))

(defun dape--config-diff (pre-eval post-eval)
  "Create a diff of PRE-EVAL and POST-EVAL configs."
  (cl-loop for (key value) on post-eval by 'cddr
           unless (equal (dape--config-eval-value (plist-get pre-eval key) t)
                         value)
           append (list key value)))

(defun dape--config-to-string (name pre-eval-config post-eval-config)
  "Create string from NAME, PRE-EVAL-CONFIG and POST-EVAL-CONFIG."
  (let ((config-diff (dape--config-diff pre-eval-config
                                        post-eval-config)))
    (concat (format "%s" name)
            (and-let* ((config-diff) (config-str (prin1-to-string config-diff)))
              (format " %s"
                      (substring config-str
                                 1
                                 (1- (length config-str))))))))

(defun dape--read-config ()
  "Read config name and options."
  (if (null dape-configs)
      (customize-variable 'dape-configs)
    (let ((candidate
           (completing-read "Dape config: "
                            (append
                             (mapcan
                              (lambda (name-config)
                                (let* ((config (cdr name-config))
                                       (modes (plist-get config 'modes)))
                                  (when (apply 'provided-mode-derived-p major-mode modes)
                                    (list (car name-config)))))
                              dape-configs)
                             dape--config-history)
                            nil nil nil 'dape-history)))
      (if-let ((config
                (alist-get (intern candidate) dape-configs)))
          (list (intern candidate) config)
        (dape--config-from-string candidate)))))

;;; Hover

(defun dape-hover-function (cb)
  "Hook function to produce doc strings for `eldoc'.
On success calles CB with the doc string.
See `eldoc-documentation-functions', for more infomation."
  (when-let ((symbol (thing-at-point 'symbol)))
    (dape--evaluate-expression (dape--live-process)
                               (plist-get (dape--current-stack-frame) :id)
                               (substring-no-properties symbol)
                               "hover"
                               (dape--callback
                                (when success
                                  (funcall cb
                                           (dape--variable-string
                                            (plist-put body :name symbol))))))
    t))

(defun dape--add-eldoc-hook ()
  "Add `dape-hover-function' from eldoc hook."
  (add-hook 'eldoc-documentation-functions #'dape-hover-function nil t))

(defun dape--remove-eldoc-hook ()
  "Remove `dape-hover-function' from eldoc hook."
  (remove-hook 'eldoc-documentation-functions #'dape-hover-function t))


;;; UI

(defun dape--update-stack-pointers ()
  "Update stack pointers."
  (dape--remove-stack-pointers)
  (when-let ((current-thread (dape--current-thread)))
    (dape--place-stack-pointers current-thread)))

(defun dape--update-inline-variables ()
  "Update inline variables from current stack frame."
  (dape--variable-remove-overlays)
  (when-let ((stack-frame (dape--current-stack-frame)))
    (dolist (scope (plist-get stack-frame :scopes))
      (dape--create-scope-overlays scope))))


(defun dape--update-widgets ()
  "Update *info-buffer* widgets."
  (dape--info-update-widget dape--threads-widget
                            dape--stack-widget
                            dape--watched-widget
                            dape--breakpoints-widget)
  (dape--info-update-scope-widget))

(defun dape--update-state (msg)
  "Update Dape mode line with MSG."
  (setq dape--state msg)
  (force-mode-line-update t))

(defun dape--mode-line-format ()
  "Format Dape mode line."
  (format "Dape:%s"
          (propertize
           (or (and (dape--live-process t)
                    (or dape--state
                        "unknown"))
               "not running")
           'face 'mode-line-emphasis)))

(add-to-list 'mode-line-misc-info
             `(dape--process
               (" [" (:eval (dape--mode-line-format)) "] ")))


;;; Keymaps

(defvar dape-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'dape)
    (define-key map "d" #'dape)
    (define-key map "p" #'dape-pause)
    (define-key map "c" #'dape-continue)
    (define-key map "n" #'dape-next)
    (define-key map "s" #'dape-step-in)
    (define-key map "o" #'dape-step-out)
    (define-key map "r" #'dape-restart)
    (define-key map "i" #'dape-info)
    (define-key map "R" #'dape-repl)
    (define-key map "m" #'dape-read-memory)
    (define-key map "l" #'dape-log-breakpoint)
    (define-key map "e" #'dape-expression-breakpoint)
    (define-key map "b" #'dape-toggle-breakpoint)
    (define-key map "B" #'dape-remove-all-breakpoints)
    (define-key map "t" #'dape-select-thread)
    (define-key map "S" #'dape-select-stack)
    (define-key map "w" #'dape-watch-dwim)
    (define-key map "q" #'dape-quit)
    map))

(dolist (cmd '(dape-pause
               dape-continue
               dape-next
               dape-step-in
               dape-step-out
               dape-restart
               dape-log-breakpoint
               dape-expression-breakpoint
               dape-toggle-breakpoint
               dape-remove-all-breakpoints
               dape-watch-dwim))
  (put cmd 'repeat-map 'dape-global-map))

(global-set-key dape-key-prefix dape-global-map)

(provide 'dape)

;;; dape.el ends here
