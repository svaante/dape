;;; dape.el --- Debug Adapter Protocol for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.3.0
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

;; Package looks is heavily inspired by gdb-mi.el

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
(require 'gdb-mi)
(require 'tramp)


;;; Custom
(defgroup dape nil
  "Debug Adapter Protocol for Emacs."
  :prefix "dape-"
  :group 'applications)

(defcustom dape-adapter-dir
  (file-name-as-directory (concat user-emacs-directory "debug-adapters"))
  "Directory to store downloaded adapters in."
  :type 'string)

(defcustom dape-configs
  `(,@(let ((codelldb
             `(ensure dape-ensure-command
               command ,(file-name-concat dape-adapter-dir
                                          "codelldb"
                                          "extension"
                                          "adapter"
                                          "codelldb")
               port :autoport
               fn dape-config-autoport
               :type "lldb"
               :request "launch"
               :cwd dape-cwd-fn
               :program dape-find-file
               :args [])))
        `((codelldb-cc
           modes (c-mode c-ts-mode c++-mode c++-ts-mode)
           command-args ("--port" :autoport)
           ,@codelldb)
          (codelldb-rust
           modes (rust-mode rust-ts-mode)
           command-args ("--port" :autoport
                         "--settings" "{\"sourceLanguages\":[\"rust\"]}")
           ,@codelldb)))
    (cpptools
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure dape-ensure-command
     command ,(file-name-concat dape-adapter-dir
                                "cpptools"
                                "extension"
                                "debugAdapters"
                                "bin"
                                "OpenDebugAD7")
     :type "cppdbg"
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file
     :MIMode ,(seq-find 'executable-find '("lldb" "gdb")))
    (debugpy
     modes (python-mode python-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let ((python
                     (dape--config-eval-value (plist-get config 'command))))
                (unless (zerop
                         (call-process-shell-command
                          (format "%s -c \"import debugpy.adapter\"" python)))
                  (user-error "%s module debugpy is not installed" python))))
     fn (dape-config-autoport dape-config-tramp)
     command "python"
     command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
     port :autoport
     :request "launch"
     :type "executable"
     :cwd dape-cwd-fn
     :program dape-find-file-buffer-default
     :justMyCode nil
     :console "integratedTerminal"
     :showReturnValue t)
    (dlv
     modes (go-mode go-ts-mode)
     ensure dape-ensure-command
     fn (dape-config-autoport dape-config-tramp)
     command "dlv"
     command-args ("dap" "--listen" "127.0.0.1::autoport")
     command-cwd dape-cwd-fn
     port :autoport
     :request "launch"
     :type "debug"
     :cwd dape-cwd-fn
     :program dape-cwd-fn)
    (flutter
     ensure dape-ensure-command
     modes (dart-mode)
     command "flutter"
     command-args ("debug_adapter")
     command-cwd dape-cwd-fn
     :type "dart"
     :cwd dape-cwd-fn
     :program dape-find-file-buffer-default
     :toolArgs ,(lambda () (vector "-d" (read-string "Device id: "))))
    (godot
     modes (gdscript-mode)
     port 6006
     :request "launch"
     :type "server"
     :cwd dape-cwd-fn)
    ,@(let ((js-debug
             `(modes (js-mode js-ts-mode)
               ensure ,(lambda (config)
                         (dape-ensure-command config)
                         (let ((js-debug-file
                                (file-name-concat
                                 (dape--config-eval-value (plist-get config 'command-cwd))
                                 (dape--config-eval-value (car (plist-get config 'command-args))))))
                           (unless (file-exists-p js-debug-file)
                             (user-error "File %S does not exist" js-debug-file))))
               command "node"
               command-cwd ,(file-name-concat dape-adapter-dir
                                              "js-debug")
               command-args (,(file-name-concat "src" "dapDebugServer.js")
                             :autoport)
               port :autoport
               fn dape-config-autoport)))
        `((js-debug-node
           ,@js-debug
           :type "pwa-node"
           :cwd dape-cwd-fn
           :program dape-find-file-buffer-default
           :outputCapture "console"
           :sourceMapRenames t
           :pauseForSourceMap nil
           :autoAttachChildProcesses t
           :console "internalConsole"
           :killBehavior "forceful")
          (js-debug-chrome
           ,@js-debug
           :type "pwa-chrome"
           :trace t
           :url ,(lambda ()
                   (read-string "Url: "
                                "http://localhost:3000"))
           :webRoot dape-cwd-fn
           :outputCapture "console")))
    (lldb-vscode
     modes (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
     ensure dape-ensure-command
     command "lldb-vscode"
     :type "lldb-vscode"
     :cwd dape-cwd-fn
     :program dape-find-file)
    (netcoredbg
     modes (csharp-mode csharp-ts-mode)
     ensure dape-ensure-command
     command "netcoredbg"
     command-args ["--interpreter=vscode"]
     :request "launch"
     :cwd dape-cwd-fn
     :program dape-find-file
     :stopAtEntry nil)
    (rdbg
     modes (ruby-mode ruby-ts-mode)
     ensure dape-ensure-command
     command "rdbg"
     command-args ("-O" "--host" "0.0.0.0" "--port" :autoport "-c" "--" :-c)
     command-cwd (lambda () (funcall dape-cwd-fn t))
     fn ((lambda (config)
           (plist-put config 'command-args
                      (mapcar (lambda (arg)
                                (if (eq arg :-c)
                                    (plist-get config '-c)
                                  arg))
                              (plist-get config 'command-args))))
         dape-config-autoport
         dape-config-tramp)
     port :autoport
     :type "Ruby"
     ;; -- examples:
     ;; rails server
     ;; bundle exec ruby foo.rb
     ;; bundle exec rake test
     -c (lambda () (read-string "Invoke command: "))))
  "This variable holds the Dape configurations as an alist.
In this alist, the car element serves as a symbol identifying each
configuration.  Each configuration, in turn, is a property list (plist)
where keys can be symbols or keywords.

Symbol Keys (Used by Dape):
- fn: Function or list of functions, takes config and returns config.
  If list functions are applied in order. Used for hiding unnecessary
  configuration details from config history.
- ensure: Function to ensure that adapter is available.
- command: Shell command to initiate the debug adapter.
- command-args: List of string arguments for the command.
- command-cwd: Working directory for the command.
- prefix-local: Defines the source path prefix, accessible from Emacs.
- prefix-remote: Defines the source path prefix, accessible by the adapter.
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
                        ((const :tag "Ensures adapter availability" ensure) function)
                        ((const :tag "Transforms configuration at runtime" fn) (choice function (repeat function)))
                        ((const :tag "Shell command to initiate the debug adapter" command) (choice string symbol))
                        ((const :tag "List of string arguments for command" command-args) (repeat string))
                        ((const :tag "Working directory for command" command-cwd) (choice string symbol))
                        ((const :tag "Path prefix for local src paths" prefix-local) string)
                        ((const :tag "Path prefix for remote src paths" prefix-remote) string)
                        ((const :tag "Host of debug adapter" host) string)
                        ((const :tag "Port of debug adapter" port) natnum)
                        ((const :tag "Compile cmd" compile) string)
                        ((const :tag "Adapter type" :type) string)
                        ((const :tag "Request type launch/attach" :request) string)))))

(defcustom dape-commands nil
  "Default commands for `dape' completion.
Sometimes it is useful for files or directories to supply local values
for this variable.

Example value:
((codelldb-cc :program \"/home/user/project/a.out\"))"
  :type '(repeat sexp))

;; TODO Add more defaults, don't know which adapters support
;;      sourceReference
(defcustom dape-mime-mode-alist '(("text/x-lldb.disassembly" . asm-mode)
                                  ("text/javascript" . js-mode))
  "Alist of MIME types vs corresponding major mode functions.
    Each element should look like (MIME-TYPE . MODE) where
    MIME-TYPE is a string and MODE is the major mode function to
    use for buffers of this MIME type."
  :type '(alist :key-type string :value-type function))

(defcustom dape-key-prefix "\C-x\C-a"
  "Prefix of all dape commands."
  :type 'key-sequence)

(defcustom dape-display-source-buffer-action
  '(display-buffer-reuse-window
    display-buffer-same-window)
  "`display-buffer' action used when displaying source buffer."
  :type 'sexp)

(defcustom dape-buffer-window-arrangment 'left
  "Rules for display dape buffers."
  :type '(choice (const :tag "GUD gdb like" gud)
                 (const :tag "Left side" left)
                 (const :tag "Right side" right)))

(defcustom dape-stepping-granularity 'line
  "The granularity of one step in the stepping requests."
  :type '(choice (const :tag "Step statement" statement)
                 (const :tag "Step line" line)
                 (const :tag "Step instruction" instruction)))

(defcustom dape-on-start-hooks '(dape-repl dape-info)
  "Hook to run on session start."
  :type 'hook)

(defcustom dape-on-stopped-hooks '()
  "Hook to run on session stopped."
  :type 'hook)

(defcustom dape-update-ui-hooks '(dape-info-update)
  "Hook to run on ui update."
  :type 'hook)

(defcustom dape-read-memory-default-count 1024
  "The default count for `dape-read-memory'."
  :type 'natnum)

(defcustom dape-info-hide-mode-line
  (and (memql dape-buffer-window-arrangment '(left right)) t)
  "Hide mode line in dape info buffers."
  :type 'boolean)

(defcustom dape-info-variable-table-row-config `((name . 20)
                                                 (value . 50)
                                                 (type . 20))
  "Configuration for table rows of variables.

An alist that controls the display of the name, type and value of
variables.  The key controls which column to change whereas the
value determines the maximum number of characters to display in each
column.  A value of 0 means there is no limit.

Additionally, the order the element in the alist determines the
left-to-right display order of the properties."
  :type '(alist :key-type symbol :value-type integer))

(defcustom dape-info-thread-buffer-verbose-names t
  "Show long thread names in threads buffer."
  :type 'boolean)

(defcustom dape-info-thread-buffer-locations t
  "Show file information or library names in threads buffer."
  :type 'boolean)

(defcustom dape-info-thread-buffer-addresses t
  "Show addresses for thread frames in threads buffer."
  :type 'boolean)

(defcustom dape-info-stack-buffer-locations t
  "Show file information or library names in stack buffers."
  :type 'boolean)

(defcustom dape-info-stack-buffer-addresses t
  "Show frame addresses in stack buffers."
  :type 'boolean)

(defcustom dape-info-buffer-variable-format 'line
  "How variables are formatted in *dape-info* buffer."
  :type '(choice (const :tag "Truncate string at new line" line)
                 (const :tag "No formatting" nil)))

(defcustom dape-info-header-scope-max-name 15
  "Max length of scope name in `header-line-format'."
  :type 'integer)

(defcustom dape-info-file-name-max 30
  "Max length of file name in dape info buffers."
  :type 'integer)

(defcustom dape-repl-use-shorthand t
  "Dape `dape-repl-commands' can be invokend with first char of command."
  :type 'boolean)

(defcustom dape-repl-commands
  '(("debug" . dape)
    ("next" . dape-next)
    ("continue" . dape-continue)
    ("pause" . dape-pause)
    ("step" . dape-step-in)
    ("out" . dape-step-out)
    ("restart" . dape-restart)
    ("kill" . dape-kill)
    ("disconnect" . dape-disconnect-quit)
    ("quit" . dape-quit))
  "Dape commands available in REPL buffer."
  :type '(alist :key-type string
                :value-type function))

(defcustom dape-compile-fn #'compile
  "Function to run compile with."
  :type 'function)

(defcustom dape-cwd-fn #'dape--default-cwd
  "Function to get current working directory.
The function should take one optional argument and return a string
representing the absolute file path of the current working directory.
If the optional argument is non nil return path with tramp prefix
otherwise the path should be without prefix.
See `dape--default-cwd'."
  :type 'function)

(defcustom dape-compile-compile-hooks nil
  "Hook run after dape compilation succeded.
The hook is run with one argument, the compilation buffer."
  :type 'hook)

(defcustom dape--debug-on '(io info error std-server)
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
  '((t :inherit (font-lock-keyword-face)))
  "Face used to display breakpoint overlays.")

(defface dape-stack-trace
  '((t :inherit (highlight) :extend t))
  "Face used to display stack trace overlays.")

(defface dape-stack-trace-pointer
  '((t :inherit (bold default) :extend t))
  "Face used to display stack trace overlays.")

(defface dape-repl-exit-code-exit
  '((t :inherit compilation-mode-line-exit :extend t))
  "Face used in repl for exit code 0.")

(defface dape-repl-exit-code-fail
  '((t :inherit compilation-mode-line-fail :extend t))
  "Face used in repl for non 0 exit codes.")


;;; Vars

(defvar dape--config nil
  "Current session configuration plist.")
(defvar dape--timers nil
  "List of running timers.")
(defvar dape--seq 0
  "Session seq number.")
(defvar dape--cb nil
  "Hash table of request callbacks.")
(defvar dape--state nil
  "Session state.")
(defvar dape--thread-id nil
  "Selected thread id.")
(defvar dape--stack-id nil
  "Selected stack id.")
(defvar dape--capabilities nil
  "Session capabilities plist.")
(defvar dape--threads nil
  "Session plist of thread data.")
(defvar dape--source-buffers nil
  "Plist of sources reference to buffer.")
(defvar dape--breakpoints nil
  "List of session breakpoint overlays.")
(defvar dape--exceptions nil
  "List of available exceptions as plists.")
(defvar dape--watched nil
  "List of watched expressions.")
(defvar dape--modules nil
  "List of modules.")
(defvar dape--sources nil
  "List of loaded sources.")
(defvar dape--server-process nil
  "Debug adapter server process.")
(defvar dape--process nil
  "Debug adapter communications process.")
(defvar dape--parent-process nil
  "Debug adapter parent process.  Used for by startDebugging adapters.")
(defvar dape--restart-in-progress nil
  "Used for prevent adapter killing when restart request is in flight.")

(defvar-local dape--source nil
  "Store source plist in fetched source buffer.")

(defvar dape--repl-insert-text-guard nil
  "Guard var for *dape-repl* buffer text updates.")


;;; Utils

(defmacro dape--callback (&rest body)
  "Create callback lambda for `dape-request' with BODY."
  `(lambda (&optional process body success msg)
     (ignore process body success msg)
     ,@body))

(defmacro dape--with (request-fn args &rest body)
  "Call `dape-request' like REQUEST-FN with ARGS and BODY."
  (declare (indent 2))
  `(,request-fn ,@args (dape--callback ,@body)))

(defun dape--next-like-command (command)
  "Helper for interactive step like commands.
Run step like COMMAND.  If ARG is set run COMMAND ARG times."
  (if (dape--stopped-threads)
      (dape-request (dape--live-process)
                    command
                    `(,@(dape--thread-id-object)
                      ,@(when (plist-get dape--capabilities
                                         :supportsSteppingGranularity)
                          (list :granularity
                                (symbol-name dape-stepping-granularity))))
                    (dape--callback
                     (when success
                       (dape--update-state 'running)
                       (dape--remove-stack-pointers)
                       (dolist (thread dape--threads)
                         (plist-put thread :status "running"))
                       (run-hooks 'dape-update-ui-hooks))))
    (user-error "No stopped threads")))

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

(defun dape--path (path format)
  "Translate PATH to FORMAT.
Accepted FORMAT values is `local' and `remote'."
  (if-let* (((or (plist-member dape--config 'prefix-local)
                 (plist-member dape--config 'prefix-remote)))
            (prefix-local (or (plist-get dape--config 'prefix-local)
                              ""))
            (prefix-remote (or (plist-get dape--config 'prefix-remote)
                               ""))
            (mapping (pcase format
                       ('local (cons prefix-remote prefix-local))
                       ('remote (cons prefix-local prefix-remote))
                       (_ (error "Unknown format")))))
      (concat
       (cdr mapping)
       (string-remove-prefix (car mapping) path))
    path))

(defun dape--current-stack-frame ()
  "Current stack frame plist."
  (let* ((stack-frames (thread-first
                         (dape--current-thread)
                         (plist-get :stackFrames)))
         (stack-frames-with-source
          (seq-filter (lambda (stack-frame)
                        (let* ((source (plist-get stack-frame :source))
                               (path (plist-get source :path))
                               (source-reference (or (plist-get source :sourceReference) 0)))
                          (or path (not (zerop source-reference)))))
                      stack-frames)))
    (or (seq-find (lambda (stack-frame)
                    (eq (plist-get stack-frame :id)
                        dape--stack-id))
                  stack-frames-with-source)
        (car stack-frames-with-source)
        (car stack-frames))))

(defun dape--object-to-marker (plist)
  "Create marker from dap PLIST containing source information.
Note requires `dape--source-ensure' if source is by reference."
  (when-let ((source (plist-get plist :source))
             (line (or (plist-get plist :line) 1))
             (buffer
              (or (when-let* ((source-reference
                               (plist-get source :sourceReference))
                              (buffer (plist-get dape--source-buffers
                                                 source-reference))
                              ((buffer-live-p buffer)))
                    buffer)
                  (when-let* ((path (plist-get source :path))
                              (path (dape--path path 'local))
                              ((file-exists-p path))
                              (buffer (find-file-noselect path t)))
                    buffer))))
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
  (dape--with dape--source-ensure ((dape--live-process t) plist)
    (when-let* ((marker (dape--object-to-marker plist))
                (window
                 (display-buffer (marker-buffer marker)
                                 dape-display-source-buffer-action)))
      (unless no-select
        (select-window window))
      (with-current-buffer (marker-buffer marker)
        (with-selected-window window
          (goto-char (marker-position marker))
          (when pulse
            (pulse-momentary-highlight-region (line-beginning-position)
                                              (line-beginning-position 2)
                                              'next-error)))))))

(defun dape--default-cwd (&optional skip-tramp-trim)
  "Try to guess current project absolute file path.
On SKIP-TRAMP-TRIM tramp prefix is keept in path."
  (let ((root (or (when-let ((project (project-current)))
                    (expand-file-name (project-root project)))
                  default-directory)))
    (if (and (not skip-tramp-trim) (tramp-tramp-file-p root))
        (tramp-file-name-localname (tramp-dissect-file-name root))
      root)))

(defun dape-find-file (&optional default)
  "Read filename without any ignored extensions at project root.
DEFAULT specifies which file to return on empty input."
  (let* ((completion-ignored-extensions nil)
         (default-directory (funcall dape-cwd-fn t))
         (file
          (expand-file-name
           (read-file-name (if default
                               (format "Program (default %s): " default)
                             "Program: ")
                           default-directory
                           default t))))
    (if (tramp-tramp-file-p file)
        (tramp-file-name-localname (tramp-dissect-file-name file))
      file)))

(defun dape-find-file-buffer-default ()
  "Read filename at project root, defaulting to current buffer."
  (dape-find-file (buffer-file-name)))

(defun dape-read-pid ()
  "Read pid of active processes if possible."
  (if-let ((pids (list-system-processes)))
      (let ((collection
             (mapcar (lambda (pid)
                       (let ((args (alist-get 'args (process-attributes pid))))
                         (cons (concat
                                (format "%d" pid)
                                (when args
                                  (format ": %s" args)))
                               pid)))
                     pids)))
        (alist-get (completing-read "Pid: " collection)
                   collection nil nil 'equal))
    (read-number "Pid: ")))

(defun dape-config-autoport (config)
  "Replaces :autoport in CONFIG keys `command-args' and `port'.
If `port' is `:autoport' replaces with open port, if not replaces
with value of `port' instead.
Replaces symbol and string occurences of \"autoport\"."
  ;; Stolen from `Eglot'
  (let ((port (plist-get config 'port)))
    (when (eq (plist-get config 'port) :autoport)
      (let ((port-probe (make-network-process :name "dape-port-probe-dummy"
                                              :server t
                                              :host "localhost"
                                              :service 0)))
        (setq port
              (unwind-protect
                  (process-contact port-probe :service)
                (delete-process port-probe)))))
    (let ((command-args (seq-map (lambda (item)
                                   (cond
                                    ((eq item :autoport)
                                     (number-to-string port))
                                    ((stringp item)
                                     (string-replace ":autoport"
                                                     (number-to-string port)
                                                     item))))
                                 (plist-get config 'command-args))))
          (thread-first config
                        (plist-put 'port port)
                        (plist-put 'command-args command-args)))))

(defun dape-config-tramp (config)
  "Apply tramp file prefix on CONFIG if started in tramp context."
  (if-let* (((and (not (plist-get config 'prefix-local))
                  (not (plist-get config 'prefix-remote))
                  (plist-get config 'command)))
            (default-directory (or (plist-get config 'command-cwd)
                                   default-directory))
            ((tramp-tramp-file-p default-directory))
            (parts (tramp-dissect-file-name default-directory))
            (tramp-prefix
             (tramp-completion-make-tramp-file-name (tramp-file-name-method parts)
                                                    (tramp-file-name-user parts)
                                                    (tramp-file-name-host parts)
                                                    "")))
      (plist-put config 'prefix-local tramp-prefix)
    config))

(defun dape-ensure-command (config)
  "Ensure that `command' from CONFIG exist system."
  (let ((command
         (dape--config-eval-value (plist-get config 'command))))
    (unless (or (file-executable-p command)
                (executable-find command t))
      (user-error "Unable to locate %S" command))))

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
  (concat
   (string-truncate-left (file-relative-name file (plist-get dape--config :cwd))
                         dape-info-file-name-max)
   (when line
     (format ":%d" line))))

(defun dape--kill-processes ()
  "Kill all Dape related process."
  (when (hash-table-p dape--timers)
    (dolist (timer (hash-table-values dape--timers))
      (cancel-timer timer)))
  (ignore-errors
    (and dape--process
         (delete-process dape--process))
    (and dape--server-process
         (delete-process dape--server-process))
    (and dape--parent-process
         (delete-process dape--parent-process))))

(defun dape--kill-buffers (&optional skip-process-buffers)
  "Kill all Dape related buffers.
On SKIP-PROCESS-BUFFERS skip deletion of buffers which has processes."
  (thread-last (buffer-list)
               (seq-filter (lambda (buffer)
                             (unless (and skip-process-buffers
                                          (get-buffer-process buffer))
                               (string-match-p "\\*dape-.+\\*" (buffer-name buffer)))))
               (seq-do (lambda (buffer)
                         (when-let ((window (get-buffer-window buffer)))
                           (delete-window window))
                         (kill-buffer buffer)))))

(defun dape--display-buffer (buffer)
  "Display BUFFER according to `dape-buffer-window-arrangment'."
  (display-buffer
   buffer
   (let ((mode (with-current-buffer buffer major-mode)))
     (pcase dape-buffer-window-arrangment
       ((or 'left 'right)
        (cons '(display-buffer-in-side-window)
              (pcase mode
                ('dape-repl-mode '((side . bottom) (slot . -1)))
                ('shell-mode '((side . bottom) (slot . 1)))
                ((or 'dape-info-scope-mode 'dape-info-watch-mode)
                 `((side . ,dape-buffer-window-arrangment) (slot . -1)))
                ((or 'dape-info-stack-mode 'dape-info-modules-mode
                     'dape-info-sources-mode)
                 `((side . ,dape-buffer-window-arrangment) (slot . 0)))
                ((or 'dape-info-breakpoints-mode 'dape-info-threads-mode)
                 `((side . ,dape-buffer-window-arrangment) (slot . 1)))
                (_ (error "Unable to display buffer of mode `%s'" mode)))))
       ('gud
        (pcase mode
          ('dape-repl-mode
           '((display-buffer-in-side-window) (side . top) (slot . -1)))
          ('shell-mode
           '((display-buffer-reuse-window)
             (display-buffer-pop-up-window) (direction . right) (dedicated . t)))
          ((or 'dape-info-scope-mode 'dape-info-watch-mode)
           '((display-buffer-in-side-window) (side . top) (slot . 0)))
          ((or 'dape-info-stack-mode 'dape-info-modules-mode
               'dape-info-sources-mode)
           '((display-buffer-in-side-window) (side . bottom) (slot . -1)))
          ((or 'dape-info-breakpoints-mode 'dape-info-threads-mode)
           '((display-buffer-in-side-window) (side . bottom) (slot . 1)))
          (_ (error "Unable to display buffer of mode `%s'" mode))))
       (_ (user-error "Invalid value of `dape-buffer-window-arrangment'"))))))


;;; Process and parsing

;; HACK Issue #1 for some reason \r is not inserted into the parse
;;      buffer by codelldb on windows. No trace in source code.

;; Some adapters can't help them self, sending headers not in spec..
(defconst dape--content-length-re
  "Content-Length: \\([[:digit:]]+\\)\r?\n\
\\(?:.*: .*\r?\n\\)*\
\r?\n"
  "Matches debug adapter protocol header.")

(defmacro dape--debug (type string &rest objects)
  "Prints STRING of TYPE to *dape-debug*.
See `format' for STRING and OBJECTS usage.
See `dape-debug-on' for TYPE information."
  `(when (memq ,type dape--debug-on)
     (let ((objects (list ,@objects)))
       (with-current-buffer (get-buffer-create "*dape-debug*")
         (setq buffer-read-only t)
         (goto-char (point-max))
         (let ((inhibit-read-only t))
           (insert (concat (propertize (format "[%s]" (symbol-name ,type)) 'face 'match)
                           " "
                           (apply 'format ,string objects))
                   "\n"))))))

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
  "Sentinel for Dape processes."
  (unless (process-live-p process)
    ;; Flush stdout contents
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (dape--debug 'io "Flushing io buffer:\n%s" (buffer-string))))
    (dape--remove-stack-pointers)
    ;; Clean mode-line after 2 seconds
    (run-with-timer 2 nil (lambda ()
                            (unless (dape--live-process t)
                              (setq dape--process nil)
                              (force-mode-line-update t))))
    (dape--debug 'info "\nProcess %S exited with %d"
                 (process-command process)
                 (process-exit-status process))))

(defun dape--handle-object (process object)
  "Handle a incoming parsed OBJECT from PROCESS."
  (dape--debug 'io "Received:\n%S" object)
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
        (dape-handle-request process
                             (intern (plist-get object :command))
                             (plist-get object :seq)
                             (plist-get object :arguments)))
      (event
       (let ((seq (plist-get object :seq)))
         ;; netcoredbg sends seq as string for some reason
         (when (stringp seq)
           (setq seq (string-to-number seq)))
         (dape-handle-event process
                            (intern (plist-get object :event))
                            (plist-get object :body))))
      (_ (dape--debug 'info "No handler for type %s" type)))))

(defun dape--process-filter (process string)
  "Filter for Dape processes."
  (when-let (((process-live-p process))
             (input-buffer (process-buffer process))
             (buffer (current-buffer)))
    (with-current-buffer input-buffer
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (let (expecting-more-bytes start)
        (while (and (setq start (point))
                    (search-forward "Content-Length: " nil t)
                    (goto-char (match-beginning 0))
                    (search-forward-regexp dape--content-length-re
                                           (+ (point) 1000) t))
          ;; Server non dap output?
          (unless (equal start (match-beginning 0))
            (dape--debug 'std-server "%s"
                         (buffer-substring start (match-beginning 0))))
          (let ((content-length (string-to-number (match-string 1))))
            (if-let* ((expected-end
                       (byte-to-position
                        (+ content-length (position-bytes (point)))))
                      (object
                       (condition-case nil
                           (json-parse-buffer :object-type 'plist
                                              :null-object nil
                                              :false-object nil)
                         (error
                          (and
                           (dape--debug 'error
                                        "Failed to parse json from `%s`"
                                        (buffer-substring (point) expected-end))
                           nil)))))
                (with-current-buffer buffer
                  (setq expecting-more-bytes nil)
                  (dape--handle-object process object))
              (dape--debug 'info "Need more bytes")
              (setq expecting-more-bytes t))))
        (when expecting-more-bytes
          (goto-char (point-min))))
      ;; This seams like we are living a bit dangerous. If input buffer
      ;; is killed we are going to erase some random buffer
      (when (buffer-live-p input-buffer)
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
                            (when (dape--live-process t)
                              (dape--update-state 'timed-out))
                            (remhash seq dape--timers)
                            (when-let ((cb (gethash seq dape--cb)))
                              (remhash seq dape--cb)
                              (funcall cb process nil nil nil)))
                           process)
           dape--timers))

(defun dape-send-object (process &optional seq object)
  "Helper for `dape-request' to send SEQ request with OBJECT to PROCESS."
  (let* ((object (if seq (plist-put object :seq seq) object))
         (json (json-serialize object :false-object nil))
         (string (format "Content-Length: %d\r\n\r\n%s" (length json) json)))
    (dape--debug 'io "Sending:\n%S" object)
    (condition-case err
        (process-send-string process string)
      (error (dape--debug 'error "%s"
                          (error-message-string err))))))

(defun dape-request (process command arguments &optional cb skip-timeout)
  "Send request COMMAND to PROCESS with ARGUMENTS.
If CB set, invoke CB on response.
If SKIP-TIMEOUT non nil skip timeout handler creation.
See `dape--callback' for expected function signature."
  (let ((seq (setq dape--seq (1+ dape--seq)))
        (object (and arguments (list :arguments arguments))))
    (unless skip-timeout
      (dape--create-timer process seq))
    (when cb
      (puthash seq cb dape--cb))
    (dape-send-object process
                      seq
                      (thread-first object
                                    (plist-put :type "request")
                                    (plist-put :command command)))))

(defun dape--initialize (process)
  "Initialize and launch/attach session for PROCESS."
  (dape--with dape-request (process
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
                                  ))
    (if (not success)
        (dape--repl-message msg 'dape-repl-exit-code-fail)
      (setq dape--capabilities body)
      (let ((start-debugging (plist-get dape--config 'start-debugging)))
        (dape-request process
                      (or (plist-get dape--config :request) "launch")
                      (append
                       (cl-loop for (key value) on dape--config by 'cddr
                                when (keywordp key)
                                append (list key value))
                       start-debugging)
                      (dape--callback
                       ;; nil start-debugging only if started as a part of
                       ;; a start-debugging request
                       (when start-debugging
                         (plist-put dape--config 'start-debugging nil))
                       (unless success
                         (dape--repl-message msg 'dape-repl-exit-code-fail)
                         (dape-kill)))
                      ;; dlv adapter takes some time during launch request
                      'skip-timeout)))))

(defun dape--set-breakpoints-in-buffer (process buffer &optional cb)
  "Set breakpoints in BUFFER by send setBreakpoints request to PROCESS.
BREAKPOINTS is an list of breakpoint overlays.
See `dape--callback' for expected CB signature."
  (let* ((breakpoints (and (buffer-live-p buffer)
                           (alist-get buffer
                                      (seq-group-by 'overlay-buffer
                                                    dape--breakpoints))))
         (lines (mapcar (lambda (breakpoint)
                          (with-current-buffer (overlay-buffer breakpoint)
                            (line-number-at-pos (overlay-start breakpoint))))
                        breakpoints))
         (source (with-current-buffer buffer
                   (or dape--source
                       (list
                        :name (file-name-nondirectory
                               (buffer-file-name buffer))
                        :path (dape--path (buffer-file-name buffer) 'remote))))))
    (dape-request process
                  "setBreakpoints"
                  (list
                   :source source
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
                                    (run-hooks 'dape-update-ui-hooks)
                                    (funcall cb process))))

(defun dape--set-breakpoints (process cb)
  "Set breakpoints in adapter PROCESS.
See `dape--callback' for expected CB signature."
  (if-let ((buffers
            (thread-last dape--breakpoints
                         (seq-group-by 'overlay-buffer)
                         (mapcar 'car)))
           (responses 0))
      (dolist (buffer buffers)
        (dape--with dape--set-breakpoints-in-buffer (process buffer)
          (setq responses (1+ responses))
          (when (eq responses (length buffers))
            (funcall cb process nil))))
    (funcall cb process nil)))

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
   ((or (not (equal (plist-get thread :status) "stopped"))
        (plist-get thread :stackFrames)
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
    (if (or (not (numberp variables-reference))
            (zerop variables-reference)
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
                (append (when (dape--stopped-threads)
                          (list :frameId frame-id))
                        (list :expression expression
                              :context context))
                cb))

(defun dape--set-variable (process ref variable value)
  "Set VARIABLE VALUE with REF by request to PROCESS.
REF should refer to VARIABLE container.
See `dape--callback' for expected CB signature."
  (cond
   ((and (plist-get dape--capabilities :supportsSetVariable)
         (numberp ref))
    (dape--with dape-request
        (process
         "setVariable"
         (list
          :variablesReference ref
          :name (plist-get variable :name)
          :value value))
      (if (not success)
          (message "%s" msg)
        (plist-put variable :variables nil)
        (cl-loop for (key value) on body by 'cddr
                 do (plist-put variable key value))
        (run-hooks 'dape-update-ui-hooks))))
   ((and (plist-get dape--capabilities :supportsSetExpression)
         (or (plist-get variable :evaluateName)
             (plist-get variable :name)))
    (dape--with dape-request
        (process
         "setExpression"
         (list :frameId (plist-get (dape--current-stack-frame) :id)
               :expression (or (plist-get variable :evaluateName)
                               (plist-get variable :name))
               :value value))
      (if (not success)
          (message "%s" msg)
        ;; FIXME: js-debug caches variables response for each stop
        ;; therefore it's not to just refresh all variables as it will
        ;; return the old value
        (dape--update process nil t))))
   ((user-error "Unable to set variable"))))

(defun dape--scopes (process stack-frame cb)
  "Send scopes request to PROCESS for STACK-FRAME plist.
See `dape--callback' for expected CB signature."
  (if-let ((id (plist-get stack-frame :id))
           ((not (plist-get stack-frame :scopes))))
      (dape-request process
                    "scopes"
                    (list :frameId id)
                    (dape--callback
                     (let ((scopes (cl-map 'list
                                           'identity
                                            (plist-get body :scopes))))
                       (plist-put stack-frame :scopes scopes)
                       (funcall cb process))))
    (funcall cb process)))

(defun dape--inactive-threads-stack-trace (process cb)
  (if (not dape--threads)
      (funcall cb process)
    (let ((responses 0))
      (dolist (thread dape--threads)
        (dape--with dape--stack-trace (process thread)
          (setq responses (1+ responses))
          (when (length= dape--threads responses)
            (funcall cb process)))))))

(defun dape--update (process
                     &optional skip-clear-stack-frames skip-stack-pointer-flash)
  "Update dape data and ui.
PROCESS specifies adapter process.
If SKIP-CLEAR-STACK-FRAMES not all stack frame data is cleared.  This
is usefully if only to load data for another thread."
  (let ((current-thread (dape--current-thread)))
    (unless skip-clear-stack-frames
      (dolist (thread dape--threads)
        (plist-put thread :stackFrames nil)))
    (dape--with dape--stack-trace (process current-thread)
      (dape--update-stack-pointers skip-stack-pointer-flash)
      (dape--with dape--scopes (process (dape--current-stack-frame))
        (run-hooks 'dape-update-ui-hooks)))))


;;; Incoming requests

(defun dape--response (process command seq success &optional body)
  "Send request response for COMMAND for SEQ with SUCCESS and BODY.
Adapter is identified with PROCESS."
  (dape-send-object process
                    nil
                    (append (list :type "response"
                                  :request_seq seq
                                  :success success
                                  :command command)
                            (when body
                              (list :body body)))))

(cl-defgeneric dape-handle-request (_process command _seq arguments)
  "Sink for all unsupported requests."
  (dape--debug 'info "Unhandled request '%S' with arguments %S"
               command
               arguments))

(cl-defmethod dape-handle-request (_process (_command (eql runInTerminal)) _seq arguments)
  "Handle runInTerminal requests.
Starts a new process to run process to be debugged."
  (let ((default-directory (or (plist-get arguments :cwd)
                               default-directory))
        (process-environment
         (or (cl-loop for (key value) on (plist-get arguments :env) by 'cddr
                      collect
                      (format "%s=%s"
                              (substring (format "%s" key) 1)
                              value))
             process-environment))
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
    (dape--display-buffer buffer)
    ;; For debugpy crashes if we send an response... it expects seq
    ;; in response which makes no sense
    ;; (dape--response process (symbol-name command) seq t
    ;;                 `(:processID ,pid))
    ))

(cl-defmethod dape-handle-request (process (command (eql startDebugging)) seq arguments)
  "Handle startDebugging requests.
Starts a new process as per request of the debug adapter."
  (dape--response process (symbol-name command) seq t)
  (setq dape--parent-process dape--process)
  ;; js-vscode leaves launch request un-answered
  (when (hash-table-p dape--timers)
    (dolist (timer (hash-table-values dape--timers))
      (cancel-timer timer)))
  (dape--create-connection (plist-put dape--config
                                      'start-debugging
                                      (plist-get arguments :configuration))))


;;; Events

(cl-defgeneric dape-handle-event (_process event body)
  "Sink for all unsupported events."
  (dape--debug 'info "Unhandled event '%S' with body %S" event body))

(cl-defmethod dape-handle-event (process (_event (eql initialized)) _body)
  "Handle initialized events."
  (dape--update-state 'initialized)
  (dape--with dape--configure-exceptions (process)
    (dape--with dape--set-breakpoints (process)
      (dape-request process "configurationDone" nil))))

(cl-defmethod dape-handle-event (process (_event (eql capabilities)) body)
  "Handle capabilities events."
  (setq dape--capabilities (plist-get body :capabilities))
  (dape--debug 'info "Capabailities recived")
  (dape--configure-exceptions process (dape--callback nil)))

(cl-defmethod dape-handle-event (_process (_event (eql module)) body)
  "Handle module events."
  (let ((reason (plist-get body :reason))
        (id (thread-first body (plist-get :module) (plist-get :id))))
    (pcase reason
      ("new"
       (setq dape--modules
             (push (plist-get body :module) dape--modules)))
      ("changed"
       (cl-loop with plist = (cl-find id dape--modules
                                      :key (lambda (module)
                                             (plist-get module :id)))
                for (key value) on body by 'cddr
                do (plist-put plist key value)))
       ("removed"
        (cl-delete id (lambda (module) (= (plist-get module :id) id))
                   :key (lambda (module) (plist-get module :id)))))))

(cl-defmethod dape-handle-event (_process (_event (eql loadedSource)) body)
  "Handle loadedSource events."
  (let ((reason (plist-get body :reason))
        (id (thread-first body (plist-get :source) (plist-get :id))))
    (pcase reason
      ("new"
       (setq dape--sources
             (push (plist-get body :source) dape--sources)))
      ("changed"
       (cl-loop with plist = (cl-find id dape--sources
                                      :key (lambda (source)
                                             (plist-get source :id)))
                for (key value) on body by 'cddr
                do (plist-put plist key value)))
      ("removed"
       (cl-delete id (lambda (source) (= (plist-get source :id) id))
                  :key (lambda (source) (plist-get source :id)))))))

(cl-defmethod dape-handle-event (_process (_event (eql process)) body)
  "Handle process events."
  (let ((start-method (format "%sed"
                              (or (plist-get body :startMethod)
                                  "start"))))
    (dape--update-state (intern start-method))
    (dape--repl-message (format "Process %s %s"
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
    (dape--update-state (intern (plist-get body :reason)))
    (push (list :status (plist-get body :reason)
                :id (plist-get body :threadId)
                :name "unnamed")
          dape--threads))
  ;; Select thread if we don't have any thread selected
  (unless dape--thread-id
    (setq dape--thread-id (plist-get body :threadId)))
  (run-hooks 'dape-update-ui-hooks))

(cl-defmethod dape-handle-event (process (_event (eql stopped)) body)
  "Handle stopped events."
  (dape--update-state 'stopped)
  (setq dape--thread-id (plist-get body :threadId))
  (dape--get-threads process
                     (plist-get body :threadId)
                     (plist-get body :allThreadsStopped)
                     (dape--callback
                      (dape--update process)))
  (when-let ((texts (seq-filter 'stringp
                                (list (plist-get body :text)
                                      (plist-get body :description)))))
    (dape--repl-message (mapconcat 'identity texts "\n")
                        (when (equal "exception"
                                   (plist-get body :reason))
                          'error)))
  (run-hooks 'dape-on-stopped-hooks))

(cl-defmethod dape-handle-event (_process (_event (eql continued)) body)
  "Handle continued events."
  (dape--update-state 'running)
  (dape--remove-stack-pointers)
  (unless dape--thread-id
    (setq dape--thread-id (plist-get body :threadId))))

(cl-defmethod dape-handle-event (_process (_event (eql output)) body)
  "Handle output events."
  (pcase (plist-get body :category)
    ("stdout"
     (dape--repl-message (plist-get body :output)))
    ("stderr"
     (dape--repl-message (plist-get body :output) 'error))
    ((or "console" "output")
     (dape--repl-message (plist-get body :output)))))

(cl-defmethod dape-handle-event (_process (_event (eql exited)) body)
  "Handle exited events."
  (dape--update-state 'exited)
  (dape--remove-stack-pointers)
  (dape--repl-message (format "* Exit code: %d *"
                              (plist-get body :exitCode))
                      (if (zerop (plist-get body :exitCode))
                          'dape-repl-exit-code-exit
                        'dape-repl-exit-code-fail)))

(cl-defmethod dape-handle-event (_process (_event (eql terminated)) _body)
  "Handle terminated events."
  (dape--update-state 'terminated)
  (dape--remove-stack-pointers)
  (dape--repl-message "* Program terminated *" 'italic)
  (unless dape--restart-in-progress
    (dape-kill)))


;;; Startup/Setup

(defun dape--setup (process config)
  "Helper for dape--start-* functions."
  (dape--remove-stack-pointers)
  ;; FIXME Cleanup source buffers in a nicer way
  (cl-loop for (_ buffer) on dape--source-buffers by 'cddr
           do (when (buffer-live-p buffer)
                (kill-buffer buffer)))
  (setq dape--config config
        dape--seq 0
        dape--timers (make-hash-table)
        dape--cb (make-hash-table)
        dape--thread-id nil
        dape--capabilities nil
        dape--threads nil
        dape--modules nil
        dape--sources nil
        dape--stack-id nil
        dape--source-buffers nil
        dape--process process
        dape--restart-in-progress nil
        dape--repl-insert-text-guard nil)
  (dape--update-state 'starting)
  (run-hook-with-args 'dape-on-start-hooks)
  (run-hooks 'dape-update-ui-hooks)
  (dape--initialize process))

(defun dape--get-buffer ()
  "Setup and get *dape-processes* buffer."
  (let ((buffer (get-buffer-create "*dape-processes*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    buffer))

(defun dape--create-connection (config)
  (dape--debug 'info "Starting new session with config:\n%S" config)
  (let ((buffer (dape--get-buffer))
        (default-directory (or (plist-get config 'command-cwd)
                               default-directory))
        (retries 30)
        process)
    (cond
     ;; socket connection
     ((plist-get config 'port)
      ;; start server
      (when (and (plist-get config 'command)
                 (not (plist-get config 'start-debugging)))
        (setq dape--server-process
              (make-process :name "Dape adapter"
                            :command (cons (plist-get config 'command)
                                           (cl-map 'list 'identity
                                                   (plist-get config 'command-args)))
                            :buffer buffer
                            :sentinel 'dape--process-sentinel
                            :filter (lambda (_process string)
                                      (dape--debug 'std-server
                                                   "Server stdout:\n%s"
                                                   string))
                            :noquery t
                            :file-handler t))
        (dape--debug 'info "Server process started %S"
                     (process-command dape--server-process))
        ;; FIXME Why do I need this?
        (when (file-remote-p default-directory)
          (sleep-for 0 300)))
      ;; connect to server
      (let ((host (or (plist-get config 'host) "localhost")))
        (while (and (not process)
                    (> retries 0))
          (ignore-errors
            (setq process
                  (make-network-process :name "Dape adapter connection"
                                        :buffer buffer
                                        :host host
                                        :coding 'utf-8-emacs-unix
                                        :service (plist-get config 'port)
                                        :sentinel 'dape--process-sentinel
                                        :filter 'dape--process-filter
                                        :noquery t)))
          (sleep-for 0 100)
          (setq retries (1- retries)))
        (if (zerop retries)
            (progn (dape-kill)
                   (user-error "Unable to connect to server %s:%d"
                               host
                               (plist-get config 'port)))
          (dape--debug 'info "Connection to server established %s:%s"
                       host (plist-get config 'port)))))
     ;; stdio connection
     (t
      (setq process (make-process :name "Dape adapter"
                                  :command (cons (plist-get config 'command)
                                                 (cl-map 'list 'identity
                                                         (plist-get config 'command-args)))
                                  :connection-type 'pipe
                                  :coding 'utf-8-emacs-unix
                                  :sentinel 'dape--process-sentinel
                                  :filter 'dape--process-filter
                                  :buffer buffer
                                  :noquery t
                                  :file-handler t))
      (dape--debug 'info "Process started %S" (process-command process))))
    (dape--setup process config)))


;;; Commands

(defun dape-next ()
  "Step one line (skip functions)."
  (interactive)
  (dape--next-like-command "next"))

(defun dape-step-in ()
  "Steps into function/method. If not possible behaves like `dape-next'."
  (interactive)
  (dape--next-like-command "stepIn"))

(defun dape-step-out ()
  "Steps out of function/method. If not possible behaves like `dape-next'."
  (interactive)
  (dape--next-like-command "stepOut"))

(defun dape-continue ()
  "Resumes execution."
  (interactive)
  (unless (dape--stopped-threads)
    (user-error "No stopped threads"))
  (dape-request (dape--live-process)
                "continue"
                (dape--thread-id-object)
                (dape--callback
                 (when success
                   (dape--update-state 'running)
                   (dape--remove-stack-pointers)
                   (dolist (thread dape--threads)
                     (plist-put thread :status "running"))
                   (run-hooks 'dape-update-ui-hooks)))))

(defun dape-pause ()
  "Pause execution."
  (interactive)
  (when (eq dape--state 'stopped)
    ;; cpptools crashes on pausing an paused thread
    (user-error "Thread already is stopped"))
  (dape-request (dape--live-process) "pause" (dape--thread-id-object)))

(defun dape-restart ()
  "Restart last debug session started."
  (interactive)
  (when (hash-table-p dape--timers)
    (dolist (timer (hash-table-values dape--timers))
      (cancel-timer timer)))
  (dape--remove-stack-pointers)
  (cond
   ((and (dape--live-process t)
         (plist-get dape--capabilities :supportsRestartRequest))
    (setq dape--threads nil)
    (setq dape--thread-id nil)
    (setq dape--restart-in-progress t)
    (dape-request dape--process "restart" nil
                  (dape--callback
                   (setq dape--restart-in-progress nil))))
   ((and dape--config)
    (dape dape--config))
   ((user-error "Unable to derive session to restart, run `dape'"))))

(defun dape-kill (&optional cb with-disconnect)
  "Kill debug session.
CB will be called after adapter termination.
With WITH-DISCONNECT use disconnect instead of terminate
used internally as a fallback to terminate."
  (interactive)
  (when (hash-table-p dape--timers)
    (dolist (timer (hash-table-values dape--timers))
      (cancel-timer timer)))
  (cond
   ((and (not with-disconnect)
         (dape--live-process t)
         (plist-get dape--capabilities
                    :supportsTerminateRequest))
    (dape-request dape--process
                  "terminate"
                  nil
                  (dape--callback
                   (if (not success)
                       (dape-kill cb 'with-disconnect)
                     (dape--kill-processes)
                     (when cb
                       (funcall cb nil))))))
   ((dape--live-process t)
    (dape-request dape--process
                  "disconnect"
                  `(:restart nil .
                    ,(when (plist-get dape--capabilities
                                      :supportTerminateDebuggee)
                       (list :terminateDebuggee t)))
                  (dape--callback
                   (dape--kill-processes)
                   (when cb
                     (funcall cb nil)))))
   (t
    (dape--kill-processes)
    (when cb
      (funcall cb nil)))))

(defun dape-disconnect-quit ()
  "Kill adapter but try to keep debuggee live.
This will leave a decoupled debuggee process with no debugge
 connection."
  (interactive)
  (dape--kill-buffers 'skip-process-buffers)
  (dape-request (dape--live-process)
                "disconnect"
                (list :terminateDebuggee nil)
                (dape--callback
                 (dape--kill-processes)
                 (dape--kill-buffers))))

(defun dape-quit ()
  "Kill debug session and kill related dape buffers."
  (interactive)
  (dape--kill-buffers 'skip-process-buffers)
  (dape-kill (dape--callback
              (dape--kill-buffers))))

(defun dape-breakpoint-toggle ()
  "Add or remove breakpoint at current line.
Will remove log or expression breakpoint at line added with
`dape-breakpoint-log' and/or `dape-breakpoint-expression'."
  (interactive)
  (if (dape--breakpoints-at-point '(dape-log-message dape-expr-message))
      (dape-breakpoint-remove-at-point '(dape-log-message dape-expr-message))
    (dape--breakpoint-place)))

(defun dape-breakpoint-log (log-message)
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
    (dape--breakpoint-remove prev-log-breakpoint t))
  (unless (string-empty-p log-message)
    (dape--breakpoint-place log-message)))

(defun dape-breakpoint-expression (expr-message)
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
    (dape--breakpoint-remove prev-expr-breakpoint t))
  (unless (string-empty-p expr-message)
    (dape--breakpoint-place nil expr-message)))

(defun dape-breakpoint-remove-at-point (&optional skip-types)
  "Remove breakpoint, log breakpoint and expression at current line.
SKIP-TYPES is a list of overlay properties to skip removal of."
  (interactive)
  (dolist (breakpoint (dape--breakpoints-at-point skip-types))
    (dape--breakpoint-remove breakpoint)))

(defun dape-breakpoint-remove-all ()
  "Remove all breakpoints."
  (interactive)
  (let ((buffers-breakpoints (seq-group-by 'overlay-buffer
                                           dape--breakpoints)))
    (dolist (buffer-breakpoints buffers-breakpoints)
      (pcase-let ((`(,buffer . ,breakpoints) buffer-breakpoints))
        (dolist (breakpoint breakpoints)
          (dape--breakpoint-remove breakpoint t))
        (when-let ((process (dape--live-process t)))
          (dape--set-breakpoints-in-buffer process buffer))))))

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

(defun dape-watch-dwim (expression &optional skip-add skip-remove)
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
      (unless skip-remove
        (setq dape--watched
              (cl-remove plist dape--watched)))
    (unless skip-add
      (push (list :name expression)
            dape--watched)
      ;; FIXME don't want to have a depency on info ui in core commands
      (dape--display-buffer (dape--info-buffer 'dape-info-watch-mode))))
  (run-hooks 'dape-update-ui-hooks))

(defun dape-evaluate-expression (expression)
  "Evaluate EXPRESSION.
EXPRESSION can be an expression or adapter command, as it's evaluated in
repl context."
  (interactive
   (list (string-trim
          (read-string "Evaluate: "
                       (or (and (region-active-p)
                                (buffer-substring (region-beginning)
                                                  (region-end)))
                           (thing-at-point 'symbol))))))
  (dape--with dape--evaluate-expression ((dape--live-process)
                                         (plist-get (dape--current-stack-frame) :id)
                                         (substring-no-properties expression)
                                         "repl")
      (message "%s" (plist-get body :result))))

;;;###autoload
(defun dape (config &optional skip-compile)
  "Start debugging session.
Start a debugging session for CONFIG.
See `dape-configs' for more information on CONFIG.

When called as an interactive command, the first symbol like
is read as key in the `dape-configs' alist and rest as elements
which override value plist in `dape-configs'.

Interactive example:
  launch :program \"bin\"

Executes alist key `launch' in `dape-configs' with :program as \"bin\".

Use SKIP-COMPILE to skip compilation."
  (interactive (list (dape--read-config)))
  (dape--with dape-kill ()
    (when-let ((fn (plist-get config 'fn))
               (fns (or (and (functionp fn) (list fn))
                        (and (listp fn) fn))))
      (setq config
            (seq-reduce (lambda (config fn)
                          (funcall fn config))
                        fns (copy-tree config))))
    (dape--config-ensure config t)
    (if (and (not skip-compile) (plist-get config 'compile))
        (dape--compile config)
      (when-let ((buffer (get-buffer "*dape-debug*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer))))
      (dape--create-connection config))))


;;; Compile

(defun dape--compile-compilation-finish (buffer str)
  "Hook for `dape--compile-compilation-finish'.
Removes itself on execution."
  (remove-hook 'compilation-finish-functions #'dape--compile-compilation-finish)
  (cond
   ((equal "finished\n" str)
    (run-hook-with-args 'dape-compile-compile-hooks buffer)
    (dape dape--config 'skip-compile))
   (t
    (dape--repl-message (format "* Compilation failed %s *" str)))))

(defun dape--compile (config)
  "Start compilation for CONFIG."
  (let ((default-directory (plist-get config :cwd))
        (command (plist-get config 'compile)))
    (setq dape--config config)
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

(defun dape-mouse-breakpoint-toggle (event)
  "Toggle breakpoint at EVENT."
  (interactive "e")
  (save-selected-window
    (let ((start (event-start event)))
      (select-window (posn-window start))
      (save-excursion
        (goto-char (posn-point start))
        (dape-breakpoint-toggle)))))

(defvar dape-breakpoint-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] 'dape-mouse-breakpoint-toggle)
    (define-key map [left-margin mouse-1] 'dape-mouse-breakpoint-toggle)
    map)
  "Keymap for `dape-breakpoint-global-mode'.")

;; TODO Whould be nice if it was enabled
(define-minor-mode dape-breakpoint-global-mode
  "Adds fringe and margin breakpoint controls."
  :global t
  :lighter "dape")

(defvar dape--original-margin nil
  "Bookkeeping for buffer margin width.")

(defun dape--margin-cleanup (buffer)
  "Reset BUFFERs margin if it's unused."
  (when buffer
    (with-current-buffer buffer
      (when (and dape--original-margin ;; Buffer has been touched by Dape
                 (not (thread-last dape--breakpoints
                                   (seq-filter (lambda (ov)
                                                 (not (overlay-get ov 'after-string))))
                                   (seq-group-by 'overlay-buffer)
                                   (alist-get buffer))))
        (setq-local left-margin-width dape--original-margin
                    dape--original-margin nil)
        ;; Update margin
        (when-let ((window (get-buffer-window buffer)))
          (set-window-buffer window buffer))))))

(defun dape--overlay-icon (overlay string bitmap face)
  "Put STRING or BITMAP on OVERLAY with FACE."
  (when-let ((buffer (overlay-buffer overlay)))
    (let (before-string)
      (cond
       ((and (window-system) ;; running in term
             (not (eql (frame-parameter (selected-frame) 'left-fringe) 0)))
        (setq before-string
              (propertize " " 'display
                          `(left-fringe ,bitmap ,face))))
       (t
        (with-current-buffer buffer
          (unless dape--original-margin
            (setq-local dape--original-margin left-margin-width)
            (setq left-margin-width 2)
            (when-let ((window (get-buffer-window)))
              (set-window-buffer window buffer))))
        (setq before-string
              (propertize " " 'display `((margin left-margin)
                                         ,(propertize string 'face face))))))
      (overlay-put overlay 'before-string before-string))))

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

(defun dape--breakpoint-buffer-kill-hook (&rest _)
  "Hook to remove breakpoint on buffer killed."
  (let ((breakpoints
         (alist-get (current-buffer)
                    (seq-group-by 'overlay-buffer
                                  dape--breakpoints))))
    (dolist (breakpoint breakpoints)
      (setq dape--breakpoints (delq breakpoint dape--breakpoints)))
  (when-let ((process (dape--live-process t)))
    (dape--set-breakpoints-in-buffer process (current-buffer))))
  (run-hooks 'dape-update-ui-hooks))

(defun dape--breakpoint-place (&optional log-message expression)
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
      (dape--overlay-icon breakpoint
                          "B"
                          'breakpoint
                          'dape-breakpoint-face)))
    (overlay-put breakpoint 'modification-hooks '(dape--breakpoint-freeze))
    (push breakpoint dape--breakpoints))
  (when-let ((process (dape--live-process t)))
    (dape--set-breakpoints-in-buffer process (current-buffer)))
  (add-hook 'kill-buffer-hook 'dape--breakpoint-buffer-kill-hook nil t)
  (run-hooks 'dape-update-ui-hooks))

(defun dape--breakpoint-remove (overlay &optional skip-update)
  "Remove OVERLAY breakpoint from buffer and session.
When SKIP-UPDATE is non nil, does not notify adapter about removal."
  (setq dape--breakpoints (delq overlay dape--breakpoints))
  (when-let (((not skip-update))
             (process (dape--live-process t)))
    (dape--set-breakpoints-in-buffer process (overlay-buffer overlay)))
  (dape--margin-cleanup (overlay-buffer overlay))
  (run-hooks 'dape-update-ui-hooks)
  (delete-overlay overlay))


;;; Source buffers

(defun dape--source-ensure (process plist cb)
  "Ensure that source object in PLIST exist for PROCESS.
See `dape--callback' for expected CB signature."
  (let* ((source (plist-get plist :source))
         (path (plist-get source :path))
         (source-reference (plist-get source :sourceReference))
         (buffer (plist-get dape--source-buffers source-reference)))
    (cond
     ((or (and path (file-exists-p (dape--path path 'local)))
          (and buffer (buffer-live-p buffer)))
      (funcall cb process))
     ((and (numberp source-reference) (> source-reference 0))
      (dape--with dape-request (process
                                "source"
                                (list
                                 :source source
                                 :sourceReference source-reference))
        (unless success
          (dape--repl-message (format "%s" msg) 'warning))
        (when-let ((content (plist-get body :content))
                   (buffer
                    (generate-new-buffer (format "*dape-source %s*"
                                                 (plist-get source :name)))))
          (setq dape--source-buffers
                (plist-put dape--source-buffers
                           (plist-get source :sourceReference) buffer))
          (with-current-buffer buffer
            (if-let* ((mime (plist-get body :mimeType))
                      (mode (alist-get mime dape-mime-mode-alist nil nil 'equal)))
                (unless (eq major-mode mode)
                  (funcall mode))
              (message "Unknown mime type %s, see `dape-mime-mode-alist'" (plist-get body :mimeType)))
            (setq-local buffer-read-only t
                        dape--source source)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content))
            (goto-char (point-min)))
          (funcall cb process)))))))


;;; Stack pointers

(defvar dape--stack-position (make-marker)
  "Dape stack position for marker `overlay-arrow-variable-list'")

(defun dape--remove-stack-pointers ()
  "Remove stack pointer marker."
  (when-let ((buffer (marker-buffer dape--stack-position)))
    (with-current-buffer buffer
      (dape--remove-eldoc-hook)))
  (set-marker dape--stack-position nil))

(defun dape--update-stack-pointers (&optional skip-stack-pointer-flash)
  "Update stack pointer marker."
  (dape--remove-stack-pointers)
  (when-let ((frame (dape--current-stack-frame)))
    (dape--with dape--source-ensure ((dape--live-process t) frame)
      (dape--goto-source frame (memq major-mode '(dape-repl-mode))
                         (not skip-stack-pointer-flash))
      (when-let ((marker (dape--object-to-marker frame)))
        (with-current-buffer (marker-buffer marker)
          (dape--add-eldoc-hook)
          (save-excursion
            (goto-char (marker-position marker))
            (set-marker dape--stack-position
                        (line-beginning-position))))))))

(add-to-list 'overlay-arrow-variable-list 'dape--stack-position)


;;; REPL buffer

(defvar dape--repl-prompt "> "
  "Dape repl prompt.")

(defun dape--repl-message (msg &optional face)
  "Insert MSG with FACE in *dape-repl* buffer.
Handles newline."
  (when (and (stringp msg) (not (string-empty-p msg)))
    (when (eql (aref msg (1- (length msg))) ?\n)
      (setq msg (substring msg 0 (1- (length msg)))))
    (setq msg (concat "\n" msg))
    (if (not (get-buffer-window "*dape-repl*"))
        (when (stringp msg)
          (message (format "%s" (string-trim msg))
                   'face face))
      (cond
       (dape--repl-insert-text-guard
        (run-with-timer 0.1 nil 'dape--repl-message msg))
       (t
        (let ((dape--repl-insert-text-guard t))
          (when-let ((buffer (get-buffer "*dape-repl*")))
            (with-current-buffer buffer
              (let (start)
                (if comint-last-prompt
                    (goto-char (1- (marker-position (car comint-last-prompt))))
                  (goto-char (point-max)))
                (setq start (point-marker))
                (let ((inhibit-read-only t))
                  (insert (propertize msg 'font-lock-face face)))
                (goto-char (point-max))
                ;; HACK Run hooks as if comint-output-filter was executed
                ;;      Could not get comint-output-filter to work by moving
                ;;      process marker. Comint removes forgets last prompt
                ;;      and everything goes to shit.
                (when-let ((process (get-buffer-process buffer)))
                  (set-marker (process-mark process)
                              (point-max)))
                (let ((comint-last-output-start start))
                  (run-hook-with-args 'comint-output-filter-functions msg)))))))))))

(defun dape--repl-insert-prompt ()
  "Insert `dape--repl-insert-prompt' into repl."
  (cond
   (dape--repl-insert-text-guard
    (run-with-timer 0.01 nil 'dape--repl-insert-prompt))
   (t
    (let ((dape--repl-insert-text-guard t))
      (when-let* ((buffer (get-buffer "*dape-repl*"))
                  (dummy-process (get-buffer-process buffer)))
        (comint-output-filter dummy-process dape--repl-prompt))))))

(defun dape--repl-input-sender (dummy-process input)
  "Dape repl `comint-input-sender'."
  (let (cmd)
    (cond
     ;; Run previous input
     ((and (string-empty-p input)
           (not (string-empty-p (car (ring-elements comint-input-ring)))))
      (when-let ((last (car (ring-elements comint-input-ring))))
        (message "Using last command %s" last)
        (dape--repl-input-sender dummy-process last)))
     ;; Run command from `dape-named-commands'
     ((setq cmd
            (or (alist-get input dape-repl-commands nil nil 'equal)
                (and dape-repl-use-shorthand
                     (cl-loop for (key . value) in dape-repl-commands
                              when (equal (substring key 0 1) input)
                              return value))))
      (dape--repl-insert-prompt)
      (call-interactively cmd))
     ;; Evaluate expression
     (t
      (dape--repl-insert-prompt)
      (dape--evaluate-expression (dape--live-process)
                                 (plist-get (dape--current-stack-frame) :id)
                                 (substring-no-properties input)
                                 "repl"
                                 (dape--callback
                                  (when success
                                    (dape--update process nil t))
                                  (dape--repl-message (concat
                                                       (if success
                                                           (plist-get body :result)
                                                         msg)))))))))

(defun dape--repl-completion-at-point ()
  "Completion at point function for *dape-repl* buffer."
  (when (or (symbol-at-point)
            (member (buffer-substring-no-properties (1- (point)) (point))
                    (or (plist-get dape--capabilities :completionTriggerCharacters)
                        '("."))))
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
          (when-let ((process (dape--live-process t)))
            (dape--with dape-request
                (process
                 "completions"
                 (append
                  (when (dape--stopped-threads)
                    (list :frameId
                          (plist-get (dape--current-stack-frame) :id)))
                  (list
                   :text str
                   :column column
                   :line 1)))
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
              (setq done t))
            (while-no-input
              (while (not done)
                (accept-process-output nil 0 1))))
          collection))
       :annotation-function
       (lambda (str)
         (when-let ((annotation
                     (alist-get (substring-no-properties str) collection
                                nil nil 'equal)))
           annotation))))))

(defvar dape-repl-mode nil)

(define-derived-mode dape-repl-mode comint-mode "Dape REPL"
  "Mode for *dape-repl* buffer."
  :group 'dape
  :interactive nil
  (when dape-repl-mode
    (user-error "`dape-repl-mode' all ready enabled"))
  (setq-local dape-repl-mode t
              comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t
              ;; HACK ? Always keep prompt at the bottom of the window
              scroll-conservatively 101
              comint-input-sender 'dape--repl-input-sender
              comint-prompt-regexp (concat "^" (regexp-quote dape--repl-prompt))
              comint-process-echoes nil)
  (add-hook 'completion-at-point-functions #'dape--repl-completion-at-point nil t)
  ;; Stolen from ielm
  ;; Start a dummy process just to please comint
  (unless (comint-check-proc (current-buffer))
    (let ((process
           (start-process "dape-repl" (current-buffer) nil)))
      (add-hook 'kill-buffer-hook (lambda () (delete-process process)) nil t))
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
                          dape--repl-prompt)))

(defun dape-repl ()
  "Create or select *dape-repl* buffer."
  (interactive)
  (let ((buffer-name "*dape-repl*")
        window)
    (with-current-buffer (get-buffer-create buffer-name)
      (unless dape-repl-mode
        (dape-repl-mode))
      (setq window (dape--display-buffer (current-buffer)))
      (when (called-interactively-p 'interactive)
        (select-window window)))))


;;; Info Buffers
;; TODO There is no way of turning on and off dape info
;;      To turn off remove hook but then you need to add it again
;;      Should be a global minor mode

;; TODO Becouse buttons where removed from info buffer
;;      there should be a way to controll execution by mouse

(defvar-local dape--info-buffer-related nil
  "List of related buffers.")
(defvar-local dape--info-buffer-identifier nil
  "Identifying var for buffers, used only in scope buffer.
Used there as scope index.")
(defvar-local dape--info-buffer-in-redraw nil
  "Guard for buffer `dape-info-update' fn.")

(defvar dape--info-buffers nil
  "List containing dape-info buffers, might be un-live.")

(defun dape--info-buffer-list ()
  "Returns all live `dape-info-parent-mode'."
  (setq dape--info-buffers
        (seq-filter 'buffer-live-p dape--info-buffers)))

(defun dape--info-buffer-p (mode &optional identifier)
  "Is buffer of MODE with IDENTIFIER.
Uses `dape--info-buffer-identifier' as IDENTIFIER."
  (and (eq major-mode mode)
       (or (not identifier)
           (equal dape--info-buffer-identifier identifier))))

(defun dape--info-buffer-tab (&optional reversed)
  "Select next related buffer in dape-info buffers.
REVERSED selects previous."
  (interactive)
  (unless dape--info-buffer-related
    (user-error "No related buffers for current buffer."))
  (pcase-let* ((order-fn (if reversed 'reverse 'identity))
               (`(,mode ,id)
                (thread-last (append dape--info-buffer-related
                                     dape--info-buffer-related)
                             (funcall order-fn)
                             (seq-drop-while (pcase-lambda (`(,mode ,id))
                                               (not (dape--info-buffer-p mode id))))
                             (cadr))))
    (gdb-set-window-buffer
     (dape--info-buffer mode id) t)))

(defvar dape-info-parent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>")
                (lambda () (interactive) (dape--info-buffer-tab t)))
    (define-key map "\t" 'dape--info-buffer-tab)
    map)
  "Keymap for `dape-info-parent-mode'.")

(defun dape--info-buffer-change-fn (&rest _rest)
  "Hook fn for `window-buffer-change-functions' to ensure updates."
  (dape--info-update (current-buffer)))

(define-derived-mode dape-info-parent-mode special-mode ""
  "Generic mode to derive all other Dape gud buffer modes from."
  :interactive nil
  (setq-local buffer-read-only t
              truncate-lines t
              cursor-in-non-selected-windows nil)
  (add-hook 'window-buffer-change-functions 'dape--info-buffer-change-fn
            nil 'local)
  (when dape-info-hide-mode-line
    (setq-local mode-line-format nil))
  (buffer-disable-undo))

(defun dape--info-header (name mode id help-echo mouse-face face)
  "Helper to create buffer header.
Creates header with string NAME, BUFFER-ID which is an list of
`dape-info-parent-mode' derived mode and `dape--info-buffer-identifier'
with HELP-ECHO string, MOSUE-FACE and FACE."
  (propertize name 'help-echo help-echo 'mouse-face mouse-face 'face face
              'keymap
              (gdb-make-header-line-mouse-map
	       'mouse-1
	       (lambda (event) (interactive "e")
		 (save-selected-window
		   (select-window (posn-window (event-start event)))
                   (gdb-set-window-buffer
                    (dape--info-buffer mode id) t))))))

(defun dape--info-set-header-line-format ()
  "Helper for dape info buffers to set header line.
Header line is custructed from buffer local
`dape--info-buffer-related'."
  (setq header-line-format
        (mapcan
         (pcase-lambda (`(,mode ,id ,name))
           (list
            (if (dape--info-buffer-p mode id)
                (dape--info-header name mode id nil nil 'mode-line)
              (dape--info-header name mode id "mouse-1: select"
                                 'mode-line-highlight
                                 'mode-line-inactive))
              " "))
         dape--info-buffer-related)))

(defun dape--info-buffer-update-1 (mode id &rest args)
  "Helper for `dape--info-buffer-update'.
Updates BUFFER contents with by calling `dape--info-buffer-update-contents'
with ARGS."
  (if dape--info-buffer-in-redraw
      (run-with-timer 0.01 nil
                      (lambda (mode id args)
                        (apply 'dape--info-buffer-update-1 mode id args)))
    (when-let ((buffer (dape--info-get-live-buffer mode id)))
      (let ((dape--info-buffer-in-redraw t))
        (with-current-buffer buffer
          (unless (derived-mode-p 'dape-info-parent-mode)
            (error "Trying to update non info buffer."))
          ;; Would be nice with replace-buffer-contents
          ;; But it seams to messes up string properties
          (let ((line (line-number-at-pos (point) t))
                (old-window (selected-window)))
            ;; Still don't know any better way of keeping window scroll?
            (when-let ((window (get-buffer-window buffer)))
              (select-window window))
            (save-window-excursion
              (let ((inhibit-read-only t))
                (erase-buffer)
                (apply 'dape--info-buffer-update-contents args))
              (ignore-errors
                (goto-char (point-min))
                (forward-line (1- line)))
              (dape--info-set-header-line-format))
            (when old-window
              (select-window old-window))))))))

(cl-defgeneric dape--info-buffer-update (mode &optional id)
  "Updates buffer specified by MODE and ID."
  (dape--info-buffer-update-1 mode id))

(defun dape--info-update (buffer)
  "Update dape info BUFFER."
  (apply 'dape--info-buffer-update
         (with-current-buffer buffer
           (list major-mode dape--info-buffer-identifier))))

(defun dape--info-get-live-buffer (mode &optional identifier)
  "Get live dape info buffer with MODE and IDENTIFIER."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (dape--info-buffer-p mode identifier)))
            (dape--info-buffer-list)))

(defun dape--info-buffer-name (mode &optional identifier)
  "Creates buffer name from MODE and IDENTIFIER."
  (format "*dape-info %s*"
          (pcase mode
            ('dape-info-breakpoints-mode "Breakpoints")
            ('dape-info-threads-mode "Threads")
            ('dape-info-stack-mode "Stack")
            ('dape-info-modules-mode "Modules")
            ('dape-info-sources-mode "Sources")
            ('dape-info-watch-mode "Watch")
            ;; FIXME If scope is named Scope <%s> there is trouble
            ('dape-info-scope-mode (format "Scope <%s>" identifier))
            (_ (error "Unable to create mode from %s with %s" mode identifier)))))

(defun dape--info-buffer (mode &optional identifier skip-update)
  "Get or create info buffer with MODE and IDENTIFIER.
If SKIP-UPDATE is non nil skip updating buffer contents."
  (let ((buffer
         (or (dape--info-get-live-buffer mode identifier)
             (get-buffer-create (dape--info-buffer-name mode identifier)))))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (funcall mode)
        (setq dape--info-buffer-identifier identifier)
        (push buffer dape--info-buffers)))
    (unless skip-update
      (dape--info-update buffer))
    buffer))

(defmacro dape--info-buffer-command (name properties doc &rest body)
  "Helper macro to create info command with NAME and DOC.
Gets PROPERTIES from string properties from current line and binds
them then executes BODY."
  (declare (indent defun))
  `(defun ,name (&optional event)
     ,doc
     (interactive (list last-input-event))
     (if event (posn-set-point (event-end event)))
     (let (,@properties)
       (save-excursion
         (beginning-of-line)
         ,@(mapcar (lambda (property)
                     `(setq ,property (get-text-property (point) ',property)))
                   properties))
       (if (and ,@properties)
           (progn
             ,@body)
         (error "Not recognized as %s line" 'name)))))

(defmacro dape--info-buffer-map (name fn &rest body)
  "Helper macro to create info buffer map with NAME.
FN is executed on mouse-2 and ?r, BODY is executed inside of let stmt."
  (declare (indent defun))
  `(defvar ,name
     (let ((map (make-sparse-keymap)))
       (suppress-keymap map)
       (define-key map "\r" ',fn)
       (define-key map [mouse-2] ',fn)
       (define-key map [follow-link] 'mouse-face)
       ,@body
       map)))

(defun dape-info-update ()
  "Update and display `dape-info-*' buffers."
  (dolist (buffer (dape--info-buffer-list))
    (dape--info-update buffer)))


(defun dape-info ()
  "Update and display *dape-info* buffers."
  (interactive)
  ;; Open breakpoints if not group-1 buffer displayed
  (unless (seq-find (lambda (buffer)
                      (and (get-buffer-window buffer)
                           (with-current-buffer buffer
                               (or (dape--info-buffer-p 'dape-info-breakpoints-mode)
                                   (dape--info-buffer-p 'dape-info-threads-mode)))))
                    (dape--info-buffer-list))
    (dape--display-buffer
     (dape--info-buffer 'dape-info-breakpoints-mode 'skip-update)))
  ;; Open and update stack buffer
  (unless (seq-find (lambda (buffer)
                      (and (get-buffer-window buffer)
                           (with-current-buffer buffer
                               (or (dape--info-buffer-p 'dape-info-stack-mode)
                                   (dape--info-buffer-p 'dape-info-modules-mode)
                                   (dape--info-buffer-p 'dape-info-sources-mode)))))
                    (dape--info-buffer-list))
    (dape--display-buffer
     (dape--info-buffer 'dape-info-stack-mode 'skip-update)))
  ;; Open stack 0 if not group-2 buffer displayed
  (unless (seq-find (lambda (buffer)
                      (and (get-buffer-window buffer)
                           (with-current-buffer buffer
                             (or (dape--info-buffer-p 'dape-info-scope-mode)
                                 (dape--info-buffer-p 'dape-info-watch-mode)))))
                    (dape--info-buffer-list))
    (dape--display-buffer
     (dape--info-buffer 'dape-info-scope-mode 0 'skip-update)))
  (dape-info-update))


;;; Info breakpoints buffer

(defconst dape--info-group-1-related
  '((dape-info-breakpoints-mode nil "Breakpoints")
    (dape-info-threads-mode nil "Threads"))
  "Realated buffers in group 1.")

(dape--info-buffer-command dape-info-breakpoint-goto (dape--info-breakpoint)
  "Goto breakpoint at line in dape info buffer."
  (when-let* ((buffer (overlay-buffer dape--info-breakpoint)))
    (select-window
     (display-buffer buffer dape-display-source-buffer-action))
    (goto-char (overlay-start dape--info-breakpoint))))

(dape--info-buffer-command dape-info-breakpoint-delete (dape--info-breakpoint)
  "Delete breakpoint at line in dape info buffer."
  (dape--breakpoint-remove dape--info-breakpoint)
  (dape--display-buffer (dape--info-buffer 'dape-info-breakpoints-mode)))

(dape--info-buffer-map dape-info-breakpoints-line-map dape-info-breakpoint-goto
  (define-key map "D" 'dape-info-breakpoint-delete)
  (define-key map "d" 'dape-info-breakpoint-delete))

(dape--info-buffer-command dape-info-exceptions-toggle (dape--info-exception)
  "Toggle exception at line in dape info buffer."
  (plist-put dape--info-exception :enabled
             (not (plist-get dape--info-exception :enabled)))
  (dape-info-update)
  (dape--with dape--set-exception-breakpoints ((dape--live-process))))

(dape--info-buffer-map dape-info-exceptions-line-map dape-info-exceptions-toggle)

(define-derived-mode dape-info-breakpoints-mode dape-info-parent-mode
  "Breakpoints"
  :interactive nil
  "Major mode for Dape info breakpoints."
  (setq dape--info-buffer-related dape--info-group-1-related))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-breakpoints-mode))
  (let ((table (make-gdb-table))
        (table-line 0))
    (gdb-table-add-row table '("Num" "Type" "On" "Where" "What"))
    (dolist (breakpoint (reverse dape--breakpoints))
      (when-let* ((buffer (overlay-buffer breakpoint))
                  (line (with-current-buffer buffer
                          (line-number-at-pos (overlay-start breakpoint)))))
        (setq table-line (1+ table-line))
        (gdb-table-add-row
         table
         (list
          (format "%d" table-line)
          (cond
           ((overlay-get breakpoint 'dape-log-message)
            "log")
           ((overlay-get breakpoint 'dape-expr-message)
            "condition")
           ("breakpoint"))
          ""
          (if-let (file (buffer-file-name buffer))
              (dape--format-file-line file line)
            (buffer-name buffer))
          (cond
           ((overlay-get breakpoint 'dape-log-message)
            (propertize (overlay-get breakpoint 'dape-log-message)
                        'face 'font-lock-comment-face))
           ((overlay-get breakpoint 'dape-expr-message))
           ("")))
         (list
          'dape--info-breakpoint breakpoint
          'keymap dape-info-breakpoints-line-map
          'mouse-face 'highlight
          'help-echo "mouse-2, RET: visit breakpoint"))))
    (dolist (exception dape--exceptions)
      (setq table-line (1+ table-line))
      (gdb-table-add-row table
                         (list
                          (format "%d" table-line)
                          "exception"
                          (if (plist-get exception :enabled)
                              (propertize "y" 'font-lock-face
                                          font-lock-warning-face)
                            (propertize "n" 'font-lock-face
                                        font-lock-comment-face))
                          (plist-get exception :label)
                          " ")
                         (list
                          'dape--info-exception exception
                          'mouse-face 'highlight
                          'keymap dape-info-exceptions-line-map
                          'help-echo "mouse-2, RET: toggle exception")))
    (insert (gdb-table-string table " "))))


;;; Info threads buffer

(defvar dape--info-thread-position nil
  "`dape-info-thread-mode' marker for `overlay-arrow-variable-list'")

(dape--info-buffer-command dape-info-select-thread (dape--info-thread)
  "Select thread at line in dape info buffer."
  (dape-select-thread (plist-get dape--info-thread :id)))

(defvar dape--info-threads-font-lock-keywords
  (append gdb-threads-font-lock-keywords
          '((" \\(unknown\\)"  (1 font-lock-warning-face))
            (" \\(exited\\)"  (1 font-lock-warning-face))
            (" \\(started\\)"  (1 font-lock-string-face))))
  "Keywords for `dape-info-threads-mode'.")

(dape--info-buffer-map dape-info-threads-line-map dape-info-select-thread
  ;; TODO Add bindings for individual threads.
  )

(define-derived-mode dape-info-threads-mode dape-info-parent-mode "Threads"
  "Major mode for Dape info threads."
  :interactive nil
  (setq font-lock-defaults '(dape--info-threads-font-lock-keywords)
        dape--info-thread-position (make-marker)
        dape--info-buffer-related dape--info-group-1-related)
  (add-to-list 'overlay-arrow-variable-list 'dape--info-thread-position))

(cl-defmethod dape--info-buffer-update ((mode (eql dape-info-threads-mode)) id)
  "Fetches data for `dape-info-threads-mode' and updates buffer.
Buffer is specified by MODE and ID."
  (if-let ((process (dape--live-process t))
           ((eq dape--state 'stopped)))
      (dape--with dape--inactive-threads-stack-trace (process)
        (dape--info-buffer-update-1 mode id
                                    :current-thread (dape--current-thread)))
    (dape--info-buffer-update-1 mode id :current-thread nil)))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-threads-mode) &key current-thread)
  "Updates `dape-info-threads-mode' buffer from CURRENT-THREAD."
  (set-marker dape--info-thread-position nil)
  (if (not dape--threads)
      (insert "No thread information available.")
    (let ((table (make-gdb-table)))
      (dolist (thread dape--threads)
        (gdb-table-add-row
         table
         (list
          (format "%s" (plist-get thread :id))
          (concat
           (when dape-info-thread-buffer-verbose-names
             (concat (plist-get thread :name) " "))
           (or (plist-get thread :status)
               "unknown")
           ;; Include frame information for stopped threads
           (if-let* (((equal (plist-get thread :status) "stopped"))
                     (top-stack (thread-first thread
                                              (plist-get :stackFrames)
                                              (car))))
               (concat
                " in " (plist-get top-stack :name)
                (when-let ((dape-info-thread-buffer-locations)
                           (path (thread-first top-stack
                                               (plist-get :source)
                                               (plist-get :path)
                                               (dape--path 'local)))
                           (line (plist-get top-stack :line)))
                  (concat " of " (dape--format-file-line path line)))
                (when-let ((dape-info-thread-buffer-addresses)
                           (addr
                            (plist-get top-stack :instructionPointerReference)))
                  (concat " at " addr))
                " "))))
         (list
          'dape--info-thread thread
          'mouse-face 'highlight
          'keymap dape-info-threads-line-map
          'help-echo "mouse-2, RET: select thread")))
      (insert (gdb-table-string table " "))
      (when current-thread
        (cl-loop for thread in dape--threads
                 for line from 1
                 until (eq current-thread thread)
                 finally (gdb-mark-line line dape--info-thread-position))))))


;;; Info stack buffer

(defvar dape--info-stack-position nil
  "`dape-info-stack-mode' marker for `overlay-arrow-variable-list'")

(defvar dape--info-stack-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--info-buffer-command dape-info-stack-select (dape--info-frame)
  "Select stack at line in dape info buffer."
  (dape-select-stack (plist-get dape--info-frame :id)))

(dape--info-buffer-map dape-info-stack-line-map dape-info-stack-select)

(define-derived-mode dape-info-stack-mode dape-info-parent-mode "Stack"
  "Major mode for Dape info stack."
  :interactive nil
  (setq font-lock-defaults '(dape--info-stack-font-lock-keywords)
        dape--info-stack-position (make-marker)
        dape--info-buffer-related '((dape-info-stack-mode nil "Stack")
                                    (dape-info-modules-mode nil "Modules")
                                    (dape-info-sources-mode nil "Sources")))
  (add-to-list 'overlay-arrow-variable-list 'dape--info-stack-position))

(cl-defmethod dape--info-buffer-update ((mode (eql dape-info-stack-mode)) id)
  "Fetches data for `dape-info-stack-mode' and updates buffer.
Buffer is specified by MODE and ID."
  (let ((stack-frames (plist-get (dape--current-thread) :stackFrames))
        (current-stack-frame (dape--current-stack-frame)))
    (dape--info-buffer-update-1 mode id
                                :current-stack-frame current-stack-frame
                                :stack-frames stack-frames)))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-stack-mode) &key current-stack-frame stack-frames)
  "Updates `dape-info-stack-mode' buffer.
Updates from CURRENT-STACK-FRAME STACK-FRAMES."
  (set-marker dape--info-stack-position nil)
  (cond
   ((or (not current-stack-frame)
        (not stack-frames)
        (not (eq dape--state 'stopped)))
    (insert "No stopped thread."))
   (t
    (cl-loop with table = (make-gdb-table)
             for frame in stack-frames
             for line from 1
             do
             (gdb-table-add-row
              table
              (list
               (format "%d" line)
               "in"
               (concat
                (plist-get frame :name)
                (when-let ((dape-info-stack-buffer-locations)
                           (path (thread-first frame
                                               (plist-get :source)
                                               (plist-get :path)
                                               (dape--path 'local))))
                  (concat " of "
                          (dape--format-file-line path
                                                  (plist-get frame :line))))
                (when-let ((dape-info-stack-buffer-addresses)
                           (ref
                            (plist-get frame :instructionPointerReference)))
                  (concat " at " ref))
                " "))
              (list
               'dape--info-frame frame
               'mouse-face 'highlight
               'keymap dape-info-stack-line-map
               'help-echo "mouse-2, RET: Select frame"))
             finally (insert (gdb-table-string table " ")))
    (cl-loop for stack-frame in stack-frames
             for line from 1
             until (eq current-stack-frame stack-frame)
             finally (gdb-mark-line line dape--info-stack-position)))))


;;; Info modules buffer

(defvar dape--info-modules-font-lock-keywords
  '(("^\\([^ ]+\\) "  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--info-buffer-command dape-info-modules-goto (dape--info-module)
  "Goto source."
  (if-let ((path (plist-get dape--info-module :path)))
      (pop-to-buffer (find-file-noselect path))
    (user-error "No path associated with module.")))

(dape--info-buffer-map dape-info-module-line-map dape-info-modules-goto)

(define-derived-mode dape-info-modules-mode dape-info-parent-mode "Modules"
  "Major mode for Dape info modules."
  :interactive nil
  (setq font-lock-defaults '(dape--info-modules-font-lock-keywords)
        dape--info-buffer-related '((dape-info-stack-mode nil "Stack")
                                    (dape-info-modules-mode nil "Modules")
                                    (dape-info-sources-mode nil "Sources"))))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-modules-mode))
  "Updates `dape-info-modules-mode' buffer."
  (cl-loop with table = (make-gdb-table)
           for module in (reverse dape--modules)
           do
           (gdb-table-add-row
            table
            (list
             (concat
              (plist-get module :name)
              (when-let ((path (plist-get module :path)))

                (concat " of " (dape--format-file-line path nil)))
              (when-let ((address-range (plist-get module :addressRange)))
                (concat " at "
                        address-range nil))
              " "))
             (list
              'dape--info-module module
              'mouse-face 'highlight
              'help-echo (format "mouse-2: goto module")
              'keymap dape-info-module-line-map))
           finally (insert (gdb-table-string table " "))))


;;; Info sources buffer

(dape--info-buffer-command dape-info-sources-goto (dape--info-source)
  "Goto source."
  (dape--with dape--source-ensure ((dape--live-process)
                                   (list :source dape--info-source))
    (if-let ((marker
              (dape--object-to-marker (list :source dape--info-source))))
        (pop-to-buffer (marker-buffer marker))
      (user-error "Unable to get source."))))

(dape--info-buffer-map dape-info-sources-line-map dape-info-sources-goto)

(define-derived-mode dape-info-sources-mode dape-info-parent-mode "Sources"
  "Major mode for Dape info sources."
  :interactive nil
  (setq dape--info-buffer-related '((dape-info-stack-mode nil "Stack")
                                    (dape-info-modules-mode nil "Modules")
                                    (dape-info-sources-mode nil "Sources"))))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-sources-mode))
  "Updates `dape-info-modules-mode' buffer."
  (cl-loop with table = (make-gdb-table)
           for source in (reverse dape--sources)
           do
           (gdb-table-add-row
            table
            (list
             (concat
              (plist-get source :name)
              " "))
            (list
             'dape--info-source source
             'mouse-face 'highlight
             'keymap dape-info-sources-line-map
             'help-echo "mouse-2, RET: goto source"))
           finally (insert (gdb-table-string table " "))))


;;; Info scope buffer

(defvar dape--info-expanded-p (make-hash-table :test 'equal)
  "Hash table to keep track of expanded info variables.")

(dape--info-buffer-command dape-info-scope-toggle (dape--info-path)
  "Expand or contract variable at line in dape info buffer."
  (unless (eq dape--state 'stopped)
    (user-error "No stopped threads"))
  (puthash dape--info-path (not (gethash dape--info-path dape--info-expanded-p))
           dape--info-expanded-p)
  (dape--info-buffer major-mode dape--info-buffer-identifier))

(dape--info-buffer-map dape-info-variable-prefix-map dape-info-scope-toggle)

(dape--info-buffer-command dape-info-scope-watch-dwim (dape--info-variable)
  "Watch variable or remove from watch at line in dape info buffer."
  (dape-watch-dwim (or (plist-get dape--info-variable :evaluateName)
                       (plist-get dape--info-variable :name))
                   (eq major-mode 'dape-info-watch-mode)
                   (eq major-mode 'dape-info-scope-mode))
  (gdb-set-window-buffer (dape--info-buffer 'dape-info-watch-mode) t))

(dape--info-buffer-map dape-info-variable-name-map dape-info-scope-watch-dwim)

(dape--info-buffer-command dape-info-variable-edit
  (dape--info-ref dape--info-variable)
  "Edit variable value at line in dape info buffer."
  (dape--set-variable (dape--live-process)
                       dape--info-ref
                       dape--info-variable
                       (read-string
                        (format "Set value of %s `%s' = "
                                (plist-get dape--info-variable :type)
                                (plist-get dape--info-variable :name))
                        (or (plist-get dape--info-variable :value)
                            (plist-get dape--info-variable :result)))))

(dape--info-buffer-map dape-info-variable-value-map dape-info-variable-edit)

(defvar dape-info-scope-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'dape-info-scope-toggle)
    (define-key map "W" 'dape-info-scope-watch-dwim)
    (define-key map "=" 'dape-info-variable-edit)
    map)
  "Local keymap for dape scope buffers.")

;; TODO Add bindings for adding data breakpoint

(define-derived-mode dape-info-scope-mode dape-info-parent-mode "Scope"
  "Major mode for Dape info scope."
  :interactive nil
  (setq dape--info-buffer-related '((dape-info-watch-mode nil "Watch")))
  (dape--info-set-header-line-format))

(defun dape--info-group-2-related-buffers (scopes)
  (append
   (cl-loop for scope in scopes
            for i from 0
            collect
            (list 'dape-info-scope-mode i
                  (string-truncate-left (plist-get scope :name)
                                        dape-info-header-scope-max-name)))
   '((dape-info-watch-mode nil "Watch"))))

(defun dape--info-locals-table-columns-list (alist)
  "Format and arrange the columns in locals display based on ALIST."
  ;; Stolen from gdb-mi but reimpleted due to usage of dape customs
  ;; org function `gdb-locals-table-columns-list'.
  (let (columns)
    (dolist (config dape-info-variable-table-row-config columns)
      (let* ((key  (car config))
             (max  (cdr config))
             (prop-org (alist-get key alist))
             (prop prop-org))
        (when prop-org
          (when (eq dape-info-buffer-variable-format 'line)
            (setq prop
                  (substring prop
                             0 (string-match-p "\n" prop))))
          (if (and (> max 0) (length> prop max))
              (push (propertize (string-truncate-left prop max) 'help-echo prop-org)
                    columns)
            (push prop columns)))))
    (nreverse columns)))

(defun dape--info-scope-add-variable (table object ref path)
  "Add variable OBJECT with REF and PATH to TABLE."
  ;; TODO Clean up
  (let* ((name (or (plist-get object :name) " "))
         (type (or (plist-get object :type) " "))
         (value (or (plist-get object :value)
                    (plist-get object :result)
                    " "))
         (prefix (make-string (* (1- (length path)) 2) ? ))
         (path (cons (plist-get object :name) path))
         (expanded (gethash path dape--info-expanded-p))
         row)
    (setq name
          (propertize name
                      'mouse-face 'highlight
                      'help-echo "mouse-2: create or remove watch expression"
                      'keymap dape-info-variable-name-map
                      'font-lock-face font-lock-variable-name-face)
          type
          (propertize type
                      'font-lock-face font-lock-type-face)
          value
          (propertize value
                      'mouse-face 'highlight
                      'help-echo "mouse-2: edit value"
                      'keymap dape-info-variable-value-map)
          prefix
          (concat
           (cond
            ((zerop (or (plist-get object :variablesReference) 0))
             (concat prefix " "))
            ((and expanded (plist-get object :variables))
             (propertize (concat prefix "-")
                         'mouse-face 'highlight
                         'help-echo "mouse-2: contract"
                         'keymap dape-info-variable-prefix-map))
            (t
             (propertize (concat prefix "+")
                         'mouse-face 'highlight
                         'help-echo "mouse-2: expand"
                         'keymap dape-info-variable-prefix-map)))
           " "))
    (setq row (dape--info-locals-table-columns-list
               `((name  . ,name)
                 (type  . ,type)
                 (value . ,value))))
    (setcar row (concat prefix (car row)))
    (gdb-table-add-row table row
                       (list 'dape--info-variable object
                             'dape--info-path path
                             'dape--info-ref ref))
    (when expanded
      ;; TODO Should be paged
      (dolist (variable (plist-get object :variables))
        (dape--info-scope-add-variable table
                                       variable
                                       (plist-get object :variablesReference)
                                       path)))))

(cl-defmethod dape--info-buffer-update ((mode (eql dape-info-scope-mode)) id)
  "Fetches data for `dape-info-scope-mode' and updates buffer.
Buffer is specified by MODE and ID."
  (when-let* ((process (dape--live-process t))
              (frame (dape--current-stack-frame))
              (scopes (plist-get frame :scopes))
              ;; FIXME if scope is out of range here scope list could
              ;;       have shrunk since last update and current
              ;;       scope buffer should be killed and replaced if
              ;;       if visible
              (scope (nth id scopes)))
    (dape--with dape--variables (process scope)
      (dape--with dape--variables-recursive
          (process
           scope
           (list (plist-get scope :name))
           (lambda (path object)
             (and (not (plist-get object :expensive))
                  (gethash (cons (plist-get object :name) path)
                           dape--info-expanded-p))))
        (when (and scope scopes (eq dape--state 'stopped))
          (dape--info-buffer-update-1 mode id :scope scope :scopes scopes))))))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-scope-mode) &key scope scopes)
  "Updates `dape-info-scope-mode' buffer for SCOPE, SCOPES."
  (rename-buffer (format "*dape-info %s*" (plist-get scope :name)) t)
  (setq dape--info-buffer-related
        (dape--info-group-2-related-buffers scopes))
  (cl-loop with table = (make-gdb-table)
           for object in (plist-get scope :variables)
           initially (setf (gdb-table-right-align table) t)
           do
           (dape--info-scope-add-variable table
                                          object
                                          (plist-get scope :variablesReference)
                                          (list (plist-get scope :name)))
           finally (insert (gdb-table-string table " "))))


;;; Info watch buffer

(defvar dape-info-watch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'dape-info-scope-watch-dwim)
    (define-key map "e" 'dape-info-scope-toggle)
    map)
  "Local keymap for dape watch buffer.")

(define-derived-mode dape-info-watch-mode dape-info-parent-mode "Watch"
  "Major mode for Dape info watch."
  :interactive nil
  (setq dape--info-buffer-related '((dape-info-watch-mode nil "Watch"))))

(cl-defmethod dape--info-buffer-update ((mode (eql dape-info-watch-mode)) id)
  "Fetches data for `dape-info-watch-mode' and updates buffer.
Buffer is specified by MODE and ID."
  (when-let* ((process (dape--live-process t))
              (frame (dape--current-stack-frame))
              (scopes (plist-get frame :scopes))
              (responses 0))
    (if (not dape--watched)
        (dape--info-buffer-update-1 mode id :scopes scopes)
      (dolist (plist dape--watched)
        (dape--with dape--evaluate-expression
            ((dape--live-process t)
             (plist-get frame :id)
             (plist-get plist :name)
             "watch")
          (when success
            (cl-loop for (key value) on body by 'cddr
                     do (plist-put plist key value)))
          (setq responses (1+ responses))
          (when (length= dape--watched responses)
            (dape--with dape--variables-recursive
                (process
                 (list :variables dape--watched)
                 (list "Watch")
                 (lambda (path object)
                   (and (not (plist-get object :expensive))
                        (gethash (cons (plist-get object :name) path)
                                 dape--info-expanded-p))))
              (dape--info-buffer-update-1 mode id :scopes scopes))))))))

(cl-defmethod dape--info-buffer-update-contents
  (&context (major-mode dape-info-watch-mode) &key scopes)
  "Updates `dape-info-watch-mode' buffer for SCOPES."
  (when scopes
    (setq dape--info-buffer-related
          (dape--info-group-2-related-buffers scopes)))
  (if (not dape--watched)
      (insert "No watched variable.")
    (cl-loop with table = (make-gdb-table)
             for watch in dape--watched
             initially (setf (gdb-table-right-align table) t)
             do
             (dape--info-scope-add-variable table watch
                                            'watch
                                            (list "Watch"))
             finally (insert (gdb-table-string table " ")))))


;;; Config

(defvar dape-history nil
  "History variable for `dape'.")
(defvar dape-session-history nil
  "Current sessions `dape--read-config' history.
Used to derive initial-contents in `dape--read-config'.")
(defvar dape--minibuffer-suggestions nil
  "Suggested configurations in minibuffer.")

(defun dape--plistp (object)
  "Non-nil if and only if OBJECT is a valid plist."
  (and-let* (((listp object))
             (len (length object))
             ((zerop (% len 2))))))

(defun dape--config-eval-value (value &optional skip-function for-adapter)
  "Evaluate dape config VALUE.
If SKIP-FUNCTION and VALUE is an function it is not invoked.
If FOR-ADAPTER current value is for the debug adapter.  Other rules
apply."
  (cond
   ((functionp value) (or (and skip-function value)
                          (funcall-interactively value)))
   ((dape--plistp value)
    (dape--config-eval-1 value skip-function for-adapter))
   ((vectorp value) (cl-map 'vector
                            (lambda (value)
                              (dape--config-eval-value value
                                                       skip-function
                                                       for-adapter))
                            value))
   ((and (symbolp value)
         (not (eq (symbol-value value) value)))
    (dape--config-eval-value (symbol-value value)
                             skip-function for-adapter))
   (t value)))

(defun dape--config-eval-1 (config &optional skip-functions for-adapter)
  "Helper for `dape--config-eval'."
  (cl-loop for (key value) on config by 'cddr
           append (cond
                   ((memql key '(modes fn ensure)) (list key value))
                   ((and for-adapter (not (keywordp key)))
                    (user-error "Unexpected key %S; lists of things needs be \
arrays [%S ...], if meant as an object replace (%S ...) with (:%s ...)"
                                key key key key))
                   (t (list key (dape--config-eval-value value
                                                         skip-functions
                                                         (or for-adapter
                                                             (keywordp key))))))))

(defun dape--config-eval (key options)
  "Evaluate Dape config with KEY and OPTIONS."
  (let ((base-config (alist-get key dape-configs)))
    (unless base-config
      (user-error "Unable to find `%s' in `dape-configs', available configurations: %s"
                  key (mapconcat (lambda (e) (symbol-name (car e)))
                                  dape-configs ", ")))
    (dape--config-eval-1 (seq-reduce (apply-partially 'apply 'plist-put)
                                     (seq-partition options 2)
                                     (copy-tree base-config)))))

(defun dape--config-from-string (str)
  "Parse list of name and config from STR."
  (let (name read-config base-config)
    (when (string-empty-p str)
      (user-error "Expected config name, available configurations: %s"
                  (mapconcat (lambda (e) (symbol-name (car e)))
                             dape-configs ", ")))
    (setq name (read str)
          base-config (copy-tree (alist-get name dape-configs))
          str (substring str (length (symbol-name name))))
    (unless (string-empty-p str)
      (setq read-config (read (format "(%s)" str))))
    (unless (dape--plistp read-config)
      (user-error "Bad options format, see `dape-configs'"))
    (cl-loop for (key value) on read-config by 'cddr
             do (setq base-config (plist-put base-config key value)))
    (list name base-config)))

(defun dape--config-diff (key post-eval)
  "Create a diff of config KEY and POST-EVAL config."
  (let ((base-config (alist-get key dape-configs)))
    (cl-loop for (key value) on post-eval by 'cddr
             unless (or (memql key '(modes fn ensure)) ;; Skip meta params
                        (and
                         ;; Does the key exist in `base-config'?
                         (plist-member base-config key)
                         ;; Has value changed?
                         (equal (dape--config-eval-value (plist-get base-config key)
                                                         t)
                                value)))
             append (list key value))))

(defun dape--config-to-string (key post-eval-config)
  "Create string from KEY and POST-EVAL-CONFIG."
  (let ((config-diff (dape--config-diff key post-eval-config)))
    (concat (format "%s" key)
            (and-let* ((config-diff) (config-str (prin1-to-string config-diff)))
              (format " %s"
                      (substring config-str
                                 1
                                 (1- (length config-str))))))))

(defun dape--config-ensure (config &optional signal)
  "Ensure that CONFIG is valid executable.
If SIGNAL is non nil raises an `user-error'."
  (if-let ((ensure-fn (plist-get config 'ensure)))
      (let ((default-directory (or (plist-get config 'command-cwd)
                                   default-directory)))
        (condition-case err
            (or (funcall ensure-fn config) t)
          (user-error
           (if signal (user-error (cdr err)) nil))))
    t))

(defun dape--config-mode-p (config)
  "Is CONFIG enabled for current mode."
  (let ((modes (plist-get config 'modes)))
    (or (not modes)
        (apply 'provided-mode-derived-p
               major-mode (cl-map 'list 'identity modes))
        (and (not (derived-mode-p 'prog-mode))
             (cl-some (lambda (mode)
                        (memql mode (plist-get dape--config 'modes)))
                      modes)))))

(defun dape--config-completion-at-point ()
  "Function for `completion-at-point' fn for `dape--read-config'."
  (pcase-let ((`(,key . ,args) (ignore-errors
                                 (read (format "(%s)" (thing-at-point 'line)))))
              (symbol-bounds (bounds-of-thing-at-point 'symbol))
              (line-bounds (bounds-of-thing-at-point 'line))
              (whitespace-bounds (bounds-of-thing-at-point 'whitespace)))
    (cond
     ;; Complete config key
     ((or (not key)
          (and (not args) symbol-bounds))
      (let ((bounds (or line-bounds (cons (point) (point)))))
        (list (car bounds) (cdr bounds)
              (mapcar (lambda (suggestion) (format "%s " suggestion))
                      dape--minibuffer-suggestions))))
     ;; Complete config args
     ((and (alist-get key dape-configs)
           (or (and (not (dape--plistp args))
                    symbol-bounds)
               (and (dape--plistp args)
                    whitespace-bounds)))
      (let ((args (if symbol-bounds
                      (nreverse (cdr (nreverse args)))
                    args))
            (bounds (or symbol-bounds (cons (point) (point))))
            (base-config (append (alist-get key dape-configs)
                                 (cons 'compile nil))))
        (list (car bounds) (cdr bounds)
              (cl-loop for (key _) on base-config by 'cddr
                       unless (plist-member args key)
                       when (or (eq key 'compile) (keywordp key))
                       collect (format "%s " key))))))))

(defun dape--read-config ()
  "Read config from minibuffer.
Initial contents defaults to valid configuration if there is only one
or last mode valid history item from this session.

See `dape--config-mode-p' how \"valid\" is defined."
  (let* ((from-dape-commands
          (cl-loop for (key . config) in dape-commands
                   when (dape--config-ensure config)
                   collect (dape--config-to-string key config)))
         (suggested-configs
          (cl-loop for (key . config) in dape-configs
                   when (and (dape--config-mode-p config)
                             (dape--config-ensure config))
                   collect (dape--config-to-string key nil)))
         (initial-contents
          (or
           ;; Take `dape-command' if exist
           (car from-dape-commands)
           ;; Take first valid history item from session
           (seq-find (lambda (str)
                       (ignore-errors
                         (member (thread-first (dape--config-from-string str)
                                               (car)
                                               (dape--config-to-string nil))
                                 suggested-configs)))
                     dape-session-history)
           ;; Take first suggested config if only one exist
           (and (length= suggested-configs 1)
                (car suggested-configs)))))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local dape--minibuffer-suggestions
                      (append from-dape-commands suggested-configs))
          (set-syntax-table emacs-lisp-mode-syntax-table)
          (add-hook 'completion-at-point-functions
                    #'dape--config-completion-at-point nil t))
      (pcase-let* ((str (read-from-minibuffer "Run adapter: "
                                              initial-contents
                                              (let ((map (make-sparse-keymap)))
                                                (set-keymap-parent map minibuffer-local-map)
                                                (define-key map "C-M-i" #'completion-at-point)
                                                (define-key map "\t" #'completion-at-point)
                                                map)
                                              nil 'dape-history initial-contents))
                   (`(,key ,config) (dape--config-from-string
                                     (substring-no-properties str)))
                   (evaled-config (dape--config-eval key config)))
        (setq dape-session-history
              (cons (dape--config-to-string key evaled-config)
                    dape-session-history))
        (setq dape-history
              (cons (dape--config-to-string key evaled-config)
                    dape-history))
        evaled-config))))


;;; Hover

(defun dape-hover-function (cb)
  "Hook function to produce doc strings for `eldoc'.
On success calles CB with the doc string.
See `eldoc-documentation-functions', for more infomation."
  (and-let* (((plist-get dape--capabilities :supportsEvaluateForHovers))
             (symbol (thing-at-point 'symbol)))
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


;;; Mode line

(defun dape--update-state (state)
  "Update Dape mode line with STATE symbol."
  (setq dape--state state)
  (force-mode-line-update t))

(defun dape--mode-line-format ()
  "Format Dape mode line."
  (concat (propertize "Dape" 'face 'font-lock-constant-face)
          ":"
          (propertize
           (format "%s" (or dape--state 'unknown))
           'face 'font-lock-doc-face)))

(add-to-list 'mode-line-misc-info
             `(dape--process
               (" [" (:eval (dape--mode-line-format)) "] ")))


;;; Keymaps

(defvar dape-global-map
  (let ((map (make-sparse-keymap)))
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
    (define-key map "l" #'dape-breakpoint-log)
    (define-key map "e" #'dape-breakpoint-expression)
    (define-key map "b" #'dape-breakpoint-toggle)
    (define-key map "B" #'dape-breakpoint-remove-all)
    (define-key map "t" #'dape-select-thread)
    (define-key map "S" #'dape-select-stack)
    (define-key map "w" #'dape-watch-dwim)
    (define-key map "D" #'dape-disconnect-quit)
    (define-key map "q" #'dape-quit)
    map))

(dolist (cmd '(dape
               dape-pause
               dape-continue
               dape-next
               dape-step-in
               dape-step-out
               dape-restart
               dape-breakpoint-log
               dape-breakpoint-expression
               dape-breakpoint-toggle
               dape-breakpoint-remove-all
               dape-watch-dwim))
  (put cmd 'repeat-map 'dape-global-map))

(global-set-key dape-key-prefix dape-global-map)


;;; Hooks

;; Cleanup process before bed time
(add-hook 'kill-emacs-hook
          (defun dape-kill-busy-wait ()
            (let (done)
              (dape-kill
               (dape--callback
                (setq done t)))
              ;; Busy wait for response at least 2 seconds
              (cl-loop with max-iterations = 20
                       for i from 1 to max-iterations
                       until done
                       do (accept-process-output nil 0.1)
                       finally (unless done
                                 (dape--kill-processes))))))

(provide 'dape)

;;; dape.el ends here
