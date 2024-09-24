;;; dape.el --- Debug Adapter Protocol for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.15.0
;; Homepage: https://github.com/svaante/dape
;; Package-Requires: ((emacs "29.1") (jsonrpc "1.0.25"))

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

;; Dape is a debug adapter client for Emacs.  The debug adapter
;; protocol, much like its more well-known counterpart, the language
;; server protocol, aims to establish a common API for programming
;; tools.  However, instead of functionalities such as code
;; completions, it provides a standardized interface for debuggers.

;; To begin a debugging session, invoke the `dape' command.  In the
;; minibuffer prompt, enter a debug adapter configuration name from
;; `dape-configs'.

;; For complete functionality, make sure to enable `eldoc-mode' in your
;; source buffers and `repeat-mode' for more pleasant key mappings.

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
(require 'hexl)
(require 'tramp)
(require 'jsonrpc)
(require 'eglot) ;; jdtls config


;;; Obsolete aliases
(define-obsolete-variable-alias 'dape-buffer-window-arrangment 'dape-buffer-window-arrangement "0.3.0")
(define-obsolete-variable-alias 'dape-read-memory-default-count 'dape-memory-page-size "0.8.0")
(define-obsolete-variable-alias 'dape-on-start-hooks 'dape-start-hook "0.13.0")
(define-obsolete-variable-alias 'dape-on-stopped-hooks 'dape-stopped-hook "0.13.0")
(define-obsolete-variable-alias 'dape-update-ui-hooks 'dape-update-ui-hook "0.13.0")
(define-obsolete-variable-alias 'dape-compile-compile-hooks 'dape-compile-hook "0.13.0")


;;; Forward declarations
(defvar hl-line-mode)
(defvar hl-line-sticky-flag)
(declare-function global-hl-line-highlight  "hl-line" ())
(declare-function hl-line-highlight         "hl-line" ())


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
  `((attach
     modes nil
     ensure (lambda (config)
              (unless (plist-get config 'port)
                (user-error "Missing `port' property")))
     host "localhost"
     :request "attach")
    (launch
     modes nil
     command-cwd dape-command-cwd
     ensure (lambda (config)
              (unless (plist-get config 'command)
                (user-error "Missing `command' property")))
     :request "launch")
    ,(let* ((extension-directory
             (expand-file-name
              (file-name-concat dape-adapter-dir "bash-debug" "extension")))
            (bashdb-dir (file-name-concat extension-directory "bashdb_dir")))
       `(bash-debug
         modes (sh-mode bash-ts-mode)
         ensure (lambda (config)
                  (dape-ensure-command config)
                  (let ((dap-debug-server-path
                         (car (plist-get config 'command-args))))
                    (unless (file-exists-p dap-debug-server-path)
                      (user-error "File %S does not exist" dap-debug-server-path))))
         command "node"
         command-args (,(file-name-concat extension-directory "out" "bashDebug.js"))
         fn (lambda (config)
              (thread-first config
                            (plist-put :pathBashdbLib ,bashdb-dir)
                            (plist-put :pathBashdb (file-name-concat ,bashdb-dir "bashdb"))
                            (plist-put :env `(:BASHDB_HOME ,,bashdb-dir . ,(plist-get config :env)))))
         :type "bashdb"
         :cwd dape-cwd
         :program dape-buffer-default
         :args []
         :pathBash "bash"
         :pathCat "cat"
         :pathMkfifo "mkfifo"
         :pathPkill "pkill"))
    ,@(let ((codelldb
             `( ensure dape-ensure-command
                command-cwd dape-command-cwd
                command ,(file-name-concat dape-adapter-dir
                                           "codelldb"
                                           "extension"
                                           "adapter"
                                           "codelldb")
                port :autoport
                :type "lldb"
                :request "launch"
                :cwd "."))
            (common `(:args [] :stopOnEntry nil)))
        `((codelldb-cc
           modes (c-mode c-ts-mode c++-mode c++-ts-mode)
           command-args ("--port" :autoport)
           ,@codelldb
           :program "a.out"
           ,@common)
          (codelldb-rust
           modes (rust-mode rust-ts-mode)
           command-args ("--port" :autoport
                         "--settings" "{\"sourceLanguages\":[\"rust\"]}")
           ,@codelldb
           :program (lambda ()
                      (file-name-concat "target" "debug"
                                        (thread-first (dape-cwd)
                                                      (directory-file-name)
                                                      (file-name-split)
                                                      (last)
                                                      (car))))
           ,@common)))
    (cpptools
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     ensure dape-ensure-command
     command-cwd dape-command-cwd
     command ,(file-name-concat dape-adapter-dir
                                "cpptools"
                                "extension"
                                "debugAdapters"
                                "bin"
                                "OpenDebugAD7")
     fn (lambda (config)
          ;; For MI=GDB the :program path need to be absolute
          (let ((program (plist-get config :program)))
            (if (file-name-absolute-p program)
                config
              (thread-last (tramp-file-local-name (dape--guess-root config))
                           (expand-file-name program)
                           (plist-put config :program)))))
     :type "cppdbg"
     :request "launch"
     :cwd "."
     :program "a.out"
     :MIMode ,(seq-find 'executable-find '("lldb" "gdb")))
    ,@(let ((debugpy
             `( modes (python-mode python-ts-mode)
                ensure (lambda (config)
                         (dape-ensure-command config)
                         (let ((python (dape-config-get config 'command)))
                           (unless (zerop
                                    (call-process-shell-command
                                     (format "%s -c \"import debugpy.adapter\"" python)))
                             (user-error "%s module debugpy is not installed" python))))
                command "python"
                command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                port :autoport
                :request "launch"
                :type "python"
                :cwd dape-cwd))
            (common
             `( :args []
                :justMyCode nil
                :console "integratedTerminal"
                :showReturnValue t
                :stopOnEntry nil)))
        `((debugpy ,@debugpy
                   :program dape-buffer-default
                   ,@common)
          (debugpy-module ,@debugpy
                          :module (lambda ()
                                    (thread-first default-directory
                                                  (directory-file-name)
                                                  (file-name-split)
                                                  (last)
                                                  (car)))
                          ,@common)))
    (dlv
     modes (go-mode go-ts-mode)
     ensure dape-ensure-command
     command "dlv"
     command-args ("dap" "--listen" "127.0.0.1::autoport")
     command-cwd dape-command-cwd
     port :autoport
     :request "launch"
     :type "debug"
     :cwd "."
     :program ".")
    (flutter
     ensure dape-ensure-command
     modes (dart-mode)
     command "flutter"
     command-args ("debug_adapter")
     command-cwd dape-command-cwd
     :type "dart"
     :cwd "."
     :program "lib/main.dart"
     :toolArgs ["-d" "all"])
    (gdb
     ensure (lambda (config)
              (dape-ensure-command config)
              (let* ((default-directory
                      (or (dape-config-get config 'command-cwd)
                          default-directory))
                     (command (dape-config-get config 'command))
                     (output (shell-command-to-string (format "%s --version" command)))
                     (version (save-match-data
                                (when (string-match "GNU gdb \\(?:(.*) \\)?\\([0-9.]+\\)" output)
                                  (string-to-number (match-string 1 output))))))
                (unless (>= version 14.1)
                  (user-error "Requires gdb version >= 14.1"))))
     modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     command-cwd dape-command-cwd
     command "gdb"
     command-args ("--interpreter=dap")
     defer-launch-attach t
     :request "launch"
     :program "a.out"
     :args []
     :stopAtBeginningOfMainSubprogram nil)
    (godot
     modes (gdscript-mode)
     port 6006
     :request "launch"
     :type "server"
     :cwd dape-cwd)
    ,@(let ((js-debug
             `( ensure ,(lambda (config)
                          (dape-ensure-command config)
                          (when-let ((runtime-executable
                                      (dape-config-get config :runtimeExecutable)))
                            (dape--ensure-executable runtime-executable))
                          (let ((dap-debug-server-path
                                 (car (plist-get config 'command-args))))
                            (unless (file-exists-p dap-debug-server-path)
                              (user-error "File %S does not exist" dap-debug-server-path))))
                command "node"
                command-args (,(expand-file-name
                                (file-name-concat dape-adapter-dir
                                                  "js-debug"
                                                  "src"
                                                  "dapDebugServer.js"))
                              :autoport)
                port :autoport)))
        `((js-debug-node
           modes (js-mode js-ts-mode)
           ,@js-debug
           :type "pwa-node"
           :cwd dape-cwd
           :program dape-buffer-default
           :console "internalConsole")
          (js-debug-ts-node
           modes (typescript-mode typescript-ts-mode)
           ,@js-debug
           :type "pwa-node"
           :runtimeExecutable "ts-node"
           :cwd dape-cwd
           :program dape-buffer-default
           :console "internalConsole")
	  (js-debug-node-attach
           modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
           ,@js-debug
           :type "pwa-node"
	   :request "attach"
	   :port 9229)
          (js-debug-chrome
           modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
           ,@js-debug
           :type "pwa-chrome"
           :url "http://localhost:3000"
           :webRoot dape-cwd)))
    ,@(let ((lldb-common
             `( modes (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode rustic-mode)
                ensure dape-ensure-command
                command-cwd dape-command-cwd
                :cwd "."
                :program "a.out")))
        `((lldb-vscode
           command "lldb-vscode"
           :type "lldb-vscode"
           ,@lldb-common)
          (lldb-dap
           command "lldb-dap"
           :type "lldb-dap"
           ,@lldb-common)))
    (netcoredbg
     modes (csharp-mode csharp-ts-mode)
     ensure dape-ensure-command
     command "netcoredbg"
     command-args ["--interpreter=vscode"]
     :request "launch"
     :cwd dape-cwd
     :program (lambda ()
                (let ((dlls
                       (file-expand-wildcards
                        (file-name-concat "bin" "Debug" "*" "*.dll"))))
                  (if dlls
                      (file-relative-name
                       (file-relative-name (car dlls)))
                    ".dll"
                    (dape-cwd))))
     :stopAtEntry nil)
    (ocamlearlybird
     ensure dape-ensure-command
     modes (tuareg-mode caml-mode)
     command "ocamlearlybird"
     command-args ("debug")
     :type "ocaml"
     :program (lambda ()
                (file-name-concat
                 (dape-cwd)
                 "_build" "default" "bin"
                 (concat
                  (file-name-base (dape-buffer-default))
                  ".bc")))
     :console "internalConsole"
     :stopOnEntry nil
     :arguments [])
    (rdbg
     modes (ruby-mode ruby-ts-mode)
     ensure dape-ensure-command
     command "rdbg"
     command-args ("-O" "--host" "0.0.0.0" "--port" :autoport "-c" "--" :-c)
     fn (lambda (config)
          (plist-put config 'command-args
                     (mapcar (lambda (arg)
                               (if (eq arg :-c)
                                   (plist-get config '-c)
                                 arg))
                             (plist-get config 'command-args))))
     port :autoport
     command-cwd dape-command-cwd
     :type "Ruby"
     ;; -- examples:
     ;; rails server
     ;; bundle exec ruby foo.rb
     ;; bundle exec rake test
     -c (lambda ()
          (format "ruby %s"
                  (or (dape-buffer-default) ""))))
    (jdtls
     modes (java-mode java-ts-mode)
     ensure (lambda (config)
              (let ((file (dape-config-get config :filePath)))
                (unless (and (stringp file) (file-exists-p file))
                  (user-error "Unable to find locate :filePath `%s'" file))
                (with-current-buffer (find-file-noselect file)
                  (unless (eglot-current-server)
                    (user-error "No eglot instance active in buffer %s" (current-buffer)))
                  (unless (seq-contains-p (eglot--server-capable :executeCommandProvider :commands)
        			          "vscode.java.resolveClasspath")
        	    (user-error "Jdtls instance does not bundle java-debug-server, please install")))))
     fn (lambda (config)
          (with-current-buffer
              (find-file-noselect (dape-config-get config :filePath))
            (if-let ((server (eglot-current-server)))
	        (pcase-let ((`[,module-paths ,class-paths]
			     (eglot-execute-command server
                                                    "vscode.java.resolveClasspath"
					            (vector (plist-get config :mainClass)
                                                            (plist-get config :projectName))))
                            (port (eglot-execute-command server
		                                         "vscode.java.startDebugSession" nil)))
	          (thread-first config
                                (plist-put 'port port)
			        (plist-put :modulePaths module-paths)
			        (plist-put :classPaths class-paths)))
              server)))
     ,@(cl-flet ((resolve-main-class (key)
                   (ignore-errors
                     (let* ((main-classes
                             (eglot-execute-command (eglot-current-server)
                                                    "vscode.java.resolveMainClass"
                                                    (file-name-nondirectory
                                                     (directory-file-name (dape-cwd)))))
                            (main-class
                             (or (seq-find (lambda(val)
                                             (equal (plist-get val :filePath)
                                                    (buffer-file-name)))
                                           main-classes)
                                 (aref main-classes 0))))
                       (plist-get main-class key)))))
         `(:filePath
           ,(lambda ()
              (or (resolve-main-class :filePath)
                  (expand-file-name (dape-buffer-default) (dape-cwd))))
           :mainClass
           ,(lambda ()
              (or (resolve-main-class :mainClass) ""))
           :projectName
           ,(lambda ()
              (or (resolve-main-class :projectName) ""))))
     :args ""
     :stopOnEntry nil
     :type "java"
     :request "launch"
     :vmArgs " -XX:+ShowCodeDetailsInExceptionMessages"
     :console "integratedConsole"
     :internalConsoleOptions "neverOpen")
    (xdebug
     modes (php-mode php-ts-mode)
     ensure (lambda (config)
              (dape-ensure-command config)
              (let ((dap-debug-server-path
                     (car (plist-get config 'command-args))))
                (unless (file-exists-p dap-debug-server-path)
                  (user-error "File %S does not exist" dap-debug-server-path))))
     command "node"
     command-args (,(expand-file-name
                     (file-name-concat dape-adapter-dir
                                       "php-debug"
                                       "extension"
                                       "out"
                                       "phpDebug.js")))
     :type "php"
     :port 9003))
  "This variable holds the dape configurations as an alist.
In this alist, the car element serves as a symbol identifying each
configuration.  Each configuration, in turn, is a property list (plist)
where keys can be symbols or keywords.

Symbol keys (Used by dape):
- fn: Function or list of functions, takes config and returns config.
  If list functions are applied in order.
  See `dape-default-config-functions'.
- ensure: Function to ensure that adapter is available.
- command: Shell command to initiate the debug adapter.
- command-args: List of string arguments for the command.
- command-cwd: Working directory for the command, if not supplied
  `default-directory' will be used.
- prefix-local: Path prefix for Emacs file access.
- prefix-remote: Path prefix for debugger file access.
- host: Host of the debug adapter.
- port: Port of the debug adapter.
- modes: List of modes where the configuration is active in `dape'
  completions.
- compile: Executes a shell command with `dape-compile-fn'.
- defer-launch-attach: If launch/attach request should be sent
  after initialize or configurationDone.  If nil launch/attach are
  sent after initialize request else it's sent after
  configurationDone.  This key exist to accommodate the two different
  interpretations of the DAP specification.
  See: GDB bug 32090.

Connection to Debug Adapter:
- If command is specified and not port, dape communicates with the
  debug adapter through stdin/stdout.
- If host and port are specified, dape connects to the debug adapter.
  If command is specified, dape waits until the command initializes
  before connecting to host and port.

Keywords in configuration:
  Keywords (symbols starting with colon) are transmitted to the
  adapter during the initialize and launch/attach requests.  Refer to
  `json-serialize' for detailed information on how dape serializes
  these keyword elements.  Dape uses nil as false.

Functions and symbols:
  - If a value is a function, its return value replaces the key's
    value before execution.  The function is called with no arguments.
  - If a value is a symbol, it resolves recursively before execution."
  :type '(alist :key-type (symbol :tag "Name")
                :value-type
                (plist :options
                       (((const :tag "List of modes where config is active in `dape' completions" modes) (repeat function))
                        ((const :tag "Ensures adapter availability" ensure) function)
                        ((const :tag "Transforms configuration at runtime" fn) (choice function (repeat function)))
                        ((const :tag "Shell command to initiate the debug adapter" command) (choice string symbol))
                        ((const :tag "List of string arguments for command" command-args) (repeat string))
                        ((const :tag "Working directory for command" command-cwd) (choice string symbol))
                        ((const :tag "Path prefix for Emacs file access" prefix-local) string)
                        ((const :tag "Path prefix for debugger file access" prefix-remote) string)
                        ((const :tag "Host of debug adapter" host) string)
                        ((const :tag "Port of debug adapter" port) natnum)
                        ((const :tag "Compile cmd" compile) string)
                        ((const :tag "Use configurationDone as trigger for launch/attach" defer-launch-attach) :type 'boolean)
                        ((const :tag "Adapter type" :type) string)
                        ((const :tag "Request type launch/attach" :request) string)))))

(defcustom dape-default-config-functions
  '(dape-config-autoport dape-config-tramp)
  "Functions applied on config before starting debugging session.
Each function is called with one argument CONFIG and should return an
PLIST of the format specified in `dape-configs'.

Functions are evaluated after functions defined in fn symbol in `dape-configs'.
See fn in `dape-configs' function signature."
  :type '(repeat function))

(defcustom dape-command nil
  "Initial contents for `dape' completion.
Sometimes it is useful for files or directories to supply local values
for this variable.

Example value:
\(codelldb-cc :program \"/home/user/project/a.out\")"
  :type 'sexp)
;;;###autoload(put 'dape-command 'safe-local-variable #'listp)

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
  `((display-buffer-use-some-window display-buffer-pop-up-window)
    (some-window
     . (lambda (&rest _)
         (cl-loop for w in (window-list nil 'skip-minibuffer) unless
                  (buffer-match-p '(or (derived-mode . dape-shell-mode)
                                       (derived-mode . dape-repl-mode)
                                       (derived-mode . dape-memory-mode)
                                       (derived-mode . dape-info-parent-mode))
                                  (window-buffer w))
                  return w))))
  "`display-buffer' action used when displaying source buffer."
  :type 'sexp)

(defcustom dape-buffer-window-arrangement 'left
  "Rules for display dape buffers."
  :type '(choice (const :tag "GUD gdb like" gud)
                 (const :tag "Left side" left)
                 (const :tag "Right side" right)))

(defcustom dape-info-buffer-window-groups
  '((dape-info-scope-mode dape-info-watch-mode)
    (dape-info-stack-mode dape-info-modules-mode dape-info-sources-mode)
    (dape-info-breakpoints-mode dape-info-threads-mode))
  "Window display rules for `dape-info-parent-mode' derived modes.
Each list of modes is displayed in the same window.  The first item of
each group is displayed by `dape-info'.  All modes doesn't need to be
present in an group."
  :type '(repeat (repeat function)))

(defcustom dape-stepping-granularity 'line
  "The granularity of one step in the stepping requests."
  :type '(choice (const :tag "Step statement" statement)
                 (const :tag "Step line" line)
                 (const :tag "Step instruction" instruction)))

(defcustom dape-stack-trace-levels 20
  "The number of stack frames fetched."
  :type 'natnum)

(defcustom dape-start-hook '(dape-repl dape-info)
  "Called when session starts."
  :type 'hook)

(defcustom dape-stopped-hook '(dape-memory-revert dape--emacs-grab-focus)
  "Called when session stopped."
  :type 'hook)

(defcustom dape-update-ui-hook '(dape-info-update)
  "Called when it's sensible to refresh UI."
  :type 'hook)

(defcustom dape-display-source-hook '()
  "Called in buffer when placing overlay arrow for stack frame."
  :type 'hook)

(defcustom dape-memory-page-size 1024
  "The bytes read with `dape-read-memory'."
  :type 'natnum)

(defcustom dape-info-hide-mode-line
  (and (memql dape-buffer-window-arrangement '(left right)) t)
  "Hide mode line in dape info buffers."
  :type 'boolean)

(defcustom dape-info-variable-table-aligned nil
  "Align columns in variable tables."
  :type 'boolean)

(defcustom dape-info-variable-table-row-config
  `((name . 20) (value . 50) (type . 20))
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
  "Show file information or library names in stack buffer."
  :type 'boolean)

(defcustom dape-info-stack-buffer-modules nil
  "Show module information in stack buffer if adapter supports it."
  :type 'boolean)

(defcustom dape-info-stack-buffer-addresses t
  "Show frame addresses in stack buffer."
  :type 'boolean)

(defcustom dape-info-buffer-variable-format 'line
  "How variables are formatted in *dape-info* buffer."
  :type '(choice (const :tag "Truncate string at new line" line)
                 (const :tag "No formatting" nil)))

(defcustom dape-info-breakpoint-source-line-max 14
  "Max length of source line in info breakpoint buffer."
  :type '(choice
          (const :tag "Don't show source line" nil)
          integer))

(defcustom dape-info-header-scope-max-name 15
  "Max length of scope name in `header-line-format'."
  :type 'integer)

(defcustom dape-info-file-name-max 25
  "Max length of file name in dape info buffers."
  :type 'integer)

(defcustom dape-breakpoint-margin-string "B"
  "String to display breakpoint in margin."
  :type 'string)

(defcustom dape-repl-use-shorthand t
  "Dape `dape-repl-commands' can be invoked with first char of command."
  :type 'boolean)

(defcustom dape-repl-commands
  '(("debug" . dape)
    ("next" . dape-next)
    ("continue" . dape-continue)
    ("pause" . dape-pause)
    ("step" . dape-step-in)
    ("out" . dape-step-out)
    ("up" . dape-stack-select-up)
    ("down" . dape-stack-select-down)
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

(defcustom dape-default-breakpoints-file
  (locate-user-emacs-file "dape-breakpoints")
  "Default file for loading and saving breakpoints.
See `dape-breakpoint-load' and `dape-breakpoint-save'."
  :type 'file)

(defcustom dape-cwd-fn #'dape--default-cwd
  "Function to get current working directory.
The function should return a string representing the absolute
file path of the current working directory, usually the current
project's root. See `dape--default-cwd'."
  :type 'function)

(defcustom dape-compile-hook nil
  "Called after dape compilation succeeded.
The hook is run with one argument, the compilation buffer."
  :type 'hook)

(defcustom dape-minibuffer-hint-ignore-properties
  '(ensure fn modes command command-args defer-launch-attach :type :request)
  "Properties to be hidden in `dape--minibuffer-hint'."
  :type '(repeat symbol))

(defcustom dape-minibuffer-hint t
  "Show `dape-configs' hints in minibuffer."
  :type 'boolean)

(defcustom dape-ui-debounce-time 0.1
  "Number of seconds to debounce `revert-buffer' for UI buffers."
  :type 'float)

(defcustom dape-request-timeout jsonrpc-default-request-timeout
  "Number of seconds until a request is deemed to be timed out."
  :type 'natnum)

(defcustom dape-debug nil
  "Print debug info in *dape-repl* *dape-connection events* buffers."
  :type 'boolean)


;;; Face
(defface dape-breakpoint-face '((t :inherit font-lock-keyword-face))
  "Face used to display breakpoint overlays.")

(defface dape-log-face '((t :inherit font-lock-string-face
                            :height 0.85 :box (:line-width -1)))
  "Face used to display log breakpoints.")

(defface dape-expression-face '((t :inherit dape-breakpoint-face
                                   :height 0.85 :box (:line-width -1)))
  "Face used to display conditional breakpoints.")

(defface dape-hits-face '((t :inherit font-lock-number-face
                             :height 0.85 :box (:line-width -1)))
  "Face used to display hits breakpoints.")

(defface dape-exception-description-face '((t :inherit (error tooltip)
                                              :extend t))
  "Face used to display exception descriptions inline.")

(defface dape-source-line-face '((t))
  "Face used to display stack frame source line overlays.")

(defface dape-repl-success-face '((t :inherit compilation-mode-line-exit
                                     :extend t))
  "Face used in repl for exit code 0.")

(defface dape-repl-error-face '((t :inherit compilation-mode-line-fail
                                   :extend t))
  "Face used in repl for non 0 exit codes.")


;;; Vars

(defvar dape-history nil
  "History variable for `dape'.")

(defvar dape--source-buffers nil
  "Plist of sources reference to buffer.")
(defvar dape--breakpoints nil
  "List of session breakpoint overlays.")
(defvar dape--exceptions nil
  "List of available exceptions as plists.")
(defvar dape--watched nil
  "List of watched expressions.")
(defvar dape--data-breakpoints nil
  "List of data breakpoints.")
(defvar dape--connection nil
  "Debug adapter connection.")
(defvar dape--connection-selected nil
  "Selected debug adapter connection.
If valid connection, this connection will be of highest priority when
querying for connections with `dape--live-connection'.")

(defvar-local dape--source nil
  "Store source plist in fetched source buffer.")

(defvar dape--repl-insert-text-guard nil
  "Guard var for *dape-repl* buffer text updates.")

(define-minor-mode dape-active-mode
  "On when dape debugging session is active.
Non interactive global minor mode."
  :global t
  :interactive nil)


;;; Utils

(defmacro dape--with-request-bind (vars fn-args &rest body)
  "Call FN with ARGS and execute BODY on callback with VARS bound.
VARS are bound from the args that the callback was invoked with.
FN-ARGS is be an cons pair as FN . ARGS, where FN is expected to
take an function as an argument at ARGS + 1.
BODY is guaranteed to be evaluated with the current buffer if it's
still live.
See `cl-destructuring-bind' for bind forms."
  (declare (indent 2))
  (let ((old-buffer (make-symbol "old-buffer")))
    `(let ((,old-buffer (current-buffer)))
       (,(car fn-args) ,@(cdr fn-args)
        (cl-function (lambda ,vars
                       (with-current-buffer (if (buffer-live-p ,old-buffer)
                                                ,old-buffer
                                              (current-buffer))
                         ,@body)))))))

(defmacro dape--with-request (fn-args &rest body)
  "Call `dape-request' like FN with ARGS and execute BODY on callback.
FN-ARGS is be an cons pair as FN . ARGS.
BODY is guaranteed to be evaluated with the current buffer if it's
still live.
See `cl-destructuring-bind' for bind forms."
  (declare (indent 1))
  `(dape--with-request-bind (&rest _) ,fn-args ,@body))

(defun dape--request-return (cb &optional error)
  "Shorthand to call CB with ERROR in an `dape-request' like way."
  (when (functionp cb)
    (funcall cb nil error)))

(defun dape--call-with-debounce (timer backoff fn)
  "Call FN with a debounce of BACKOFF seconds.
This function utilizes TIMER to store state.  It cancels the TIMER
and schedules FN to run after current time + BACKOFF seconds.
If BACKOFF is non-zero, FN will be evaluated within timer context."
  (cond
   ((zerop backoff)
    (cancel-timer timer)
    (funcall fn))
   (t
    (cancel-timer timer)
    (timer-set-time timer (timer-relative-time nil backoff))
    (timer-set-function timer fn)
    (timer-activate timer))))

(defmacro dape--with-debounce (timer backoff &rest body)
  "Eval BODY forms with a debounce of BACKOFF seconds using TIMER.
Helper macro for `dape--call-with-debounce'."
  (declare (indent 2))
  `(dape--call-with-debounce ,timer ,backoff (lambda () ,@body)))

(defun dape--next-like-command (conn command)
  "Helper for interactive step like commands.
Run step like COMMAND on CONN.  If ARG is set run COMMAND ARG times."
  (if (not (dape--stopped-threads conn))
      (user-error "No stopped threads")
    (dape--with-request-bind
        (_body error)
        (dape-request conn
                      command
                      `(,@(dape--thread-id-object conn)
                        ,@(when (dape--capable-p conn :supportsSteppingGranularity)
                            (list :granularity
                                  (symbol-name dape-stepping-granularity)))))
      (if error
          (message "Failed to \"%s\": %s" command error)
        ;; From specification [continued] event:
        ;; A debug adapter is not expected to send this event in
        ;; response to a request that implies that execution
        ;; continues, e.g. launch or continue.
        (dape-handle-event conn 'continued nil)))))

(defun dape--maybe-select-thread (conn thread-id force)
  "Maybe set selected THREAD-ID and CONN.
If FORCE is non nil force thread selection.
If thread is selected, select CONN as well if no previously connection
has been selected or if current selected connection does not have any
stopped threads.
See `dape--connection-selected'."
  (when (and thread-id
             (or force (not (dape--thread-id conn))))
    (setf (dape--thread-id conn) thread-id)
    (unless (and (member dape--connection-selected (dape--live-connections))
                 (dape--stopped-threads dape--connection-selected))
      (setq dape--connection-selected conn))))

(defun dape--threads-make-update-handle (conn)
  "Return an threads update update handle for CONN.
See `dape--threads-set-status'."
  (setf (dape--threads-update-handle conn)
        (1+ (dape--threads-update-handle conn))))

(defun dape--threads-set-status (conn thread-id all-threads status update-handle)
  "Set string STATUS thread(s) for CONN.
If THREAD-ID is non nil set status for thread with :id equal to
THREAD-ID to STATUS.
If ALL-THREADS is non nil set status of all all threads to STATUS.
Ignore status update if UPDATE-HANDLE is not the last handle created
by `dape--threads-make-update-handle'."
  (when (> update-handle (dape--threads-last-update-handle conn))
    (setf (dape--threads-last-update-handle conn) update-handle)
    (cond
     ((not status) nil)
     (all-threads
      (cl-loop for thread in (dape--threads conn)
               do (plist-put thread :status status)))
     (thread-id
      (plist-put
       (cl-find-if (lambda (thread)
                     (equal (plist-get thread :id) thread-id))
                   (dape--threads conn))
       :status status)))))

(defun dape--thread-id-object (conn)
  "Construct a thread id object for CONN."
  (when-let ((thread-id (dape--thread-id conn)))
    (list :threadId thread-id)))

(defun dape--stopped-threads (conn)
  "List of stopped threads for CONN."
  (and conn
       (mapcan (lambda (thread)
                 (when (equal (plist-get thread :status) 'stopped)
                   (list thread)))
               (dape--threads conn))))

(defun dape--current-thread (conn)
  "Current thread plist for CONN."
  (and conn
       (seq-find (lambda (thread)
                   (eq (plist-get thread :id) (dape--thread-id conn)))
                 (dape--threads conn))))

(defun dape--path (conn path format)
  "Return translate absolute PATH in FORMAT from CONN config.
Accepted FORMAT values are local and remote.
See `dape-configs' symbols prefix-local prefix-remote."
  (if-let* ((config (dape--config
                     ;; Fallback to last connection
                     (or conn dape--connection)))
            (path
             (expand-file-name
              path
              (let ((command-cwd (plist-get config 'command-cwd)))
                (pcase format
                  ('local (tramp-file-local-name command-cwd))
                  ('remote command-cwd)))))
            ((or (plist-member config 'prefix-local)
                 (plist-member config 'prefix-remote)))
            (prefix-local (or (plist-get config 'prefix-local) ""))
            (prefix-remote (or (plist-get config 'prefix-remote) ""))
            (mapping (pcase format
                       ('local (cons prefix-remote prefix-local))
                       ('remote (cons prefix-local prefix-remote))
                       (_ (error "Unknown format"))))
            ;; Substitute prefix if there is an match or nil
            ((string-prefix-p (car mapping) path)))
      (concat (cdr mapping)
              (string-remove-prefix (car mapping) path))
    path))

(defun dape--capable-p (conn thing)
  "Return non nil if CONN capable of THING."
  (eq (plist-get (dape--capabilities conn) thing) t))

(defun dape--current-stack-frame (conn)
  "Current stack frame plist for CONN."
  (let ((stack-frames (plist-get (dape--current-thread conn) :stackFrames)))
    (or (when conn
          (cl-find (dape--stack-id conn) stack-frames
                   :key (lambda (frame) (plist-get frame :id))))
        (car stack-frames))))

(defun dape--object-to-marker (conn plist)
  "Return marker created from PLIST and CONN config.
Marker is created from PLIST keys :source and :line.
Note requires `dape--source-ensure' if source is by reference."
  (when-let ((source (plist-get plist :source))
             (line (or (plist-get plist :line) 1))
             (buffer
              (cond
               ((and-let* ((ref (plist-get source :sourceReference))
                           (buffer (plist-get dape--source-buffers ref))
                           ((buffer-live-p buffer)))
                  buffer))
               ((and-let* ((path (plist-get source :path))
                           (path (dape--path conn path 'local))
                           ((file-exists-p path)))
                  (find-file-noselect path t))))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (when-let ((column (plist-get plist :column)))
          (when (> column 0)
            (forward-char (1- column))))
        (point-marker)))))

(defun dape--default-cwd ()
  "Try to guess current project absolute file path with `project'."
  (or (when-let ((project (project-current)))
        (expand-file-name (project-root project)))
      default-directory))

(defun dape-cwd ()
  "Use `dape-cwd-fn' to guess current working as local path."
  (tramp-file-local-name (funcall dape-cwd-fn)))

(defun dape-command-cwd ()
  "Use `dape-cwd-fn' to guess current working directory."
  (funcall dape-cwd-fn))

(defun dape-buffer-default ()
  "Return current buffers file name."
  (tramp-file-local-name
   (file-relative-name (buffer-file-name) (dape-command-cwd))))

(defun dape--guess-root (config)
  "Return best guess root path from CONFIG."
  (if-let* ((command-cwd (plist-get config 'command-cwd))
            ((stringp command-cwd)))
      command-cwd
    (dape-command-cwd)))

(defun dape-config-autoport (config)
  "Replace :autoport in CONFIG keys `command-args' and `port'.
If `port' is not `:autoport' return config as is."
  (when (eq (plist-get config 'port) :autoport)
    ;; Stolen from `Eglot'
    (let ((port-probe
           (make-network-process :name "dape-port-probe-dummy"
                                 :server t
                                 :host "localhost"
                                 :service 0)))
      (plist-put config
                 'port
                 (unwind-protect
                     (process-contact port-probe :service)
                   (delete-process port-probe)))))
  (when-let* ((command-args (plist-get config 'command-args))
              (port (plist-get config 'port))
              (port-string (number-to-string port)))
    (plist-put
     config
     'command-args
     (seq-map (lambda (arg)
                (cond
                 ((eq arg :autoport) port-string)
                 ((stringp arg) (string-replace ":autoport" port-string arg))
                 (t arg)))
              command-args)))
  config)

(defun dape-config-tramp (config)
  "Infer `prefix-local' and `host' on CONFIG if in tramp context.
If `tramp-tramp-file-p' is nil for command-cwd or command-cwd is nil
and `tramp-tramp-file-p' is nil for `defualt-directory' return config
as is."
  (when-let* ((default-directory
               (or (plist-get config 'command-cwd)
                   default-directory))
              ((tramp-tramp-file-p default-directory))
              (parts (tramp-dissect-file-name default-directory)))
    (when (and (not (plist-get config 'prefix-local))
               (not (plist-get config 'prefix-remote))
               (plist-get config 'command))
      (let ((prefix-local
             (tramp-completion-make-tramp-file-name
              (tramp-file-name-method parts)
              (tramp-file-name-user parts)
              (tramp-file-name-host parts)
              "")))
        (dape--repl-message
         (format "* Remote connection detected, setting %s to %S *"
                 (propertize "prefix-local"
                             'font-lock-face 'font-lock-keyword-face)
                 prefix-local))
        (plist-put config 'prefix-local prefix-local)))
    (when (and (plist-get config 'command)
               (plist-get config 'port)
               (not (plist-get config 'host))
               (equal (tramp-file-name-method parts) "ssh"))
      (let ((host (file-remote-p default-directory 'host)))
        (dape--repl-message
         (format "* Remote connection detected, setting %s to %S *"
                 (propertize "host"
                             'font-lock-face 'font-lock-keyword-face)
                 host))
        (plist-put config 'host host))))
  config)

(defun dape--ensure-executable (executable)
  "Ensure that EXECUTABLE exist on system."
  (unless (or (file-executable-p executable)
              (executable-find executable t))
    (user-error "Unable to locate %S (default-directory %s)"
                executable default-directory)))

(defun dape-ensure-command (config)
  "Ensure that `command' from CONFIG exist system."
  (dape--ensure-executable (dape-config-get config 'command)))

(defun dape--overlay-region ()
  "List of beg and end of current line."
  (list (line-beginning-position)
        (1- (line-beginning-position 2))))

(defun dape--format-file-line (file line)
  "Formats FILE and LINE to string."
  (let* ((conn dape--connection)
         (config
          (and conn
               ;; If child connection check parent
               (or (and-let* ((parent (dape--parent conn)))
                     (dape--config parent))
                   (dape--config conn))))
         (root-guess (dape--guess-root config))
         ;; Normalize paths for `file-relative-name'
         (file (tramp-file-local-name file))
         (root-guess (tramp-file-local-name root-guess)))
    (concat
     (string-truncate-left (file-relative-name file root-guess)
                           dape-info-file-name-max)
     (when line
       (format ":%d" line)))))

(defun dape--kill-buffers (&optional skip-process-buffers)
  "Kill all Dape related buffers.
On SKIP-PROCESS-BUFFERS skip deletion of buffers which has processes."
  (thread-last (buffer-list)
               (seq-filter (lambda (buffer)
                             (unless (and skip-process-buffers
                                          (get-buffer-process buffer))
                               (string-match-p "\\*dape-.+\\*$"
                                               (buffer-name buffer)))))
               (seq-do (lambda (buffer)
                         (condition-case err
                             (let ((window (get-buffer-window buffer)))
                               (kill-buffer buffer)
                               (when (window-live-p window)
                                 (delete-window window)))
                           (error
                            (message (error-message-string err))))))))

(defun dape--display-buffer (buffer)
  "Display BUFFER according to `dape-buffer-window-arrangement'."
  (pcase-let* ((mode (with-current-buffer buffer major-mode))
               (group (cl-position-if (lambda (group) (memq mode group))
                                      dape-info-buffer-window-groups))
               (`(,fns . ,alist)
                (pcase dape-buffer-window-arrangement
                  ((or 'left 'right)
                   (cons '(display-buffer-in-side-window)
                         (pcase (cons mode group)
                           (`(dape-repl-mode . ,_) '((side . bottom) (slot . -1)))
                           (`(dape-shell-mode . ,_) '((side . bottom) (slot . 0)))
                           (`(,_ . 0) `((side . ,dape-buffer-window-arrangement) (slot . -1)))
                           (`(,_ . 1) `((side . ,dape-buffer-window-arrangement) (slot . 0)))
                           (`(,_ . 2) `((side . ,dape-buffer-window-arrangement) (slot . 1)))
                           (_ (error "Unable to display buffer of mode `%s'" mode)))))
                  ('gud
                   (pcase (cons mode group)
                     (`(dape-repl-mode . ,_)
                      '((display-buffer-in-side-window) (side . top) (slot . -1)))
                     (`(dape-shell-mode . ,_)
                      '((display-buffer-pop-up-window)
                        (direction . right) (dedicated . t)))
                     (`(,_ . 0)
                      '((display-buffer-in-side-window) (side . top) (slot . 0)))
                     (`(,_ . 1)
                      '((display-buffer-in-side-window) (side . bottom) (slot . -1)))
                     (`(,_ . 2)
                      '((display-buffer-in-side-window) (side . bottom) (slot . 1)))
                     (_ (error "Unable to display buffer of mode `%s'" mode)))))))
    (display-buffer buffer `((display-buffer-reuse-window . ,fns) . ,alist))))

(defmacro dape--mouse-command (name doc command)
  "Create mouse command with NAME, DOC which call COMMAND."
  (declare (indent 1))
  `(defun ,name (event)
     ,doc
     (interactive "e")
     (save-selected-window
       (let ((start (event-start event)))
         (select-window (posn-window start))
         (save-excursion
           (goto-char (posn-point start))
           (call-interactively ',command))))))

(defmacro dape--buffer-map (name fn &rest body)
  "Helper macro to create info buffer map with NAME.
FN is executed on mouse-2 and \r, BODY is executed with `map' bound."
  (declare (indent defun))
  `(defvar ,name
     (let ((map (make-sparse-keymap)))
       (suppress-keymap map)
       (define-key map "\r" ',fn)
       (define-key map [mouse-2] ',fn)
       (define-key map [follow-link] 'mouse-face)
       ,@body
       map)))

(defmacro dape--command-at-line (name properties doc &rest body)
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

(defun dape--emacs-grab-focus ()
  "If `display-graphic-p' focus Emacs."
  (select-frame-set-input-focus (selected-frame)))


;;; Connection

(defun dape--live-connection (type &optional nowarn require-selected)
  "Get live connection of TYPE.
TYPE is expected to be one of the following symbols:

parent   parent connection.
last     last created child connection or parent which has an active
         thread.
running  last created child connection or parent which has an active
         thread but no stopped threads.
stopped  last created child connection or parent which has stopped
         threads.

If NOWARN is non nil does not error on no active process.
If REQUIRE-SELECTED is non nil require returned connection to be the
selected one, this has no effect when TYPE is parent.
See `dape--connection-selected'."
  (let* ((connections (dape--live-connections))
         (selected (cl-find dape--connection-selected connections))
         (ordered
          `(,@(when selected
                (list selected))
            ,@(unless (and require-selected selected)
                (reverse connections))))
         (conn
          (pcase type
            ('parent
             (car connections))
            ('last
             (seq-find 'dape--thread-id ordered))
            ('running
             (seq-find (lambda (conn)
                         (and (dape--thread-id conn)
                              (not (dape--stopped-threads conn))))
                       ordered))
            ('stopped
             (seq-find (lambda (conn)
                         (and (dape--stopped-threads conn)))
                       ordered)))))
    (unless (or nowarn conn)
      (user-error "No %s debug connection live" type))
    conn))

(defun dape--live-connections ()
  "Get all live connections."
  (cl-labels ((live-connections-1 (conn)
                (when (and conn (jsonrpc-running-p conn))
                  (cons conn
                        (mapcan #'live-connections-1
                                ;; New children are `push'ed onto the
                                ;; children list, therefore children
                                ;; are `reverse'd to guarantee LIFO
                                ;; order.
                                (reverse (dape--children conn)))))))
    (live-connections-1 dape--connection)))

(defclass dape-connection (jsonrpc-process-connection)
  ((last-id
    :initform 0
    :documentation "Used for converting JSONRPC's `id' to DAP' `seq'.")
   (n-sent-notifs
    :initform 0
    :documentation "Used for converting JSONRPC's `id' to DAP' `seq'.")
   (children
    :accessor dape--children :initarg :children :initform (list)
    :documentation "Child connections.  Used by startDebugging adapters.")
   (parent
    :accessor dape--parent :initarg :parent :initform #'ignore
    :documentation "Parent connection.  Used by startDebugging adapters.")
   (config
    :accessor dape--config :initarg :config :initform #'ignore
    :documentation "Current session configuration plist.")
   (server-process
    :accessor dape--server-process :initarg :server-process :initform #'ignore
    :documentation "Debug adapter server process.")
   (threads
    :accessor dape--threads :initform nil
    :documentation "Session plist of thread data.")
   (threads-update-handle
    :initform 0 :accessor dape--threads-update-handle
    :documentation "Current handle for updating thread state.")
   (threads-last-update-handle
    :initform 0 :accessor dape--threads-last-update-handle
    :documentation "Last handle used when updating thread state")
   (capabilities
    :accessor dape--capabilities :initform nil
    :documentation "Session capabilities plist.")
   (thread-id
    :accessor dape--thread-id :initform nil
    :documentation "Selected thread id.")
   (stack-id
    :accessor dape--stack-id :initform nil
    :documentation "Selected stack id.")
   (modules
    :accessor dape--modules :initform nil
    :documentation "List of modules.")
   (sources
    :accessor dape--sources :initform nil
    :documentation "List of loaded sources.")
   (state
    :accessor dape--state :initform nil
    :documentation "Session state.")
   (state-reason
    :accessor dape--state-reason :initform nil
    :documentation "Reason for state.")
   (exception-description
    :accessor dape--exception-description :initform nil
    :documentation "Exception description.")
   (initialized-p
    :accessor dape--initialized-p :initform nil
    :documentation "If connection has been initialized.")
   (restart-in-progress-p
    :accessor dape--restart-in-progress-p :initform nil
    :documentation "If restart request is in flight."))
  :documentation
  "Represents a DAP debugger. Wraps a process for DAP communication.")

(cl-defmethod jsonrpc-convert-to-endpoint ((conn dape-connection)
                                           message subtype)
  "Convert jsonrpc CONN MESSAGE with SUBTYPE to DAP format."
  (cl-destructuring-bind (&key method id error params
                               (result nil result-supplied-p))
      message
    (with-slots (last-id n-sent-notifs) conn
      (cond ((eq subtype 'notification)
             (cl-incf n-sent-notifs)
             `(:type "event"
                     :seq ,(+ last-id n-sent-notifs)
                     :event ,method
                     :body ,params))
            ((eq subtype 'request)
             `(:type "request"
                     :seq ,(+ (setq last-id id) n-sent-notifs)
                     :command ,method
                     ,@(when params `(:arguments ,params))))
            (error
             `(:type "response"
                     :seq ,(+ (setq last-id id) n-sent-notifs)
                     :request_seq ,last-id
                     :success :json-false
                     :message ,(plist-get error :message)
                     :body ,(plist-get error :data)))
            (t
             `(:type "response"
                     :seq ,(+ (setq last-id id) n-sent-notifs)
                     :request_seq ,last-id
                     :command ,method
                     :success t
                     ,@(and result `(:body ,result))))))))

(cl-defmethod jsonrpc-convert-from-endpoint ((_conn dape-connection) dap-message)
  "Convert JSONRPCesque DAP-MESSAGE to JSONRPC plist."
  (cl-destructuring-bind (&key type request_seq seq command arguments
                               event body &allow-other-keys)
      dap-message
    (when (stringp seq) ;; dirty dirty netcoredbg
      (setq seq (string-to-number seq)))
    (cond ((string= type "event")
           `(:method ,event :params ,body))
          ((string= type "response")
           `(:id ,request_seq :result ,dap-message))
          (command
           `(:id ,seq :method ,command :params ,arguments)))))


;;; Outgoing requests

(defconst dape--timeout-error "Request timeout"
  "Error string for request timeout.
Useful for `eq' comparison to derive request timeout error.")

(defun dape-request (conn command arguments &optional cb)
  "Send request with COMMAND and ARGUMENTS to adapter CONN.
If callback function CB is supplied, it's called on timeout
and success.

CB will be called with PLIST and ERROR.
On success, ERROR will be nil.
On failure, ERROR will be an string."
  (jsonrpc-async-request conn command arguments
                         :success-fn
                         (when (functionp cb)
                           (lambda (result)
                             (funcall cb (plist-get result :body)
                                      (unless (eq (plist-get result :success) t)
                                        (or (plist-get result :message) "")))))
                         :error-fn 'ignore ;; will never be called
                         :timeout-fn
                         (when (functionp cb)
                           (lambda ()
                             (dape--repl-message
                              (format
                               "* Command %s timed out after %d seconds, the \
timeout period is configurable with `dape-request-timeout' *"
                               command
                               dape-request-timeout)
                              'dape-repl-error-face)
                             (funcall cb nil dape--timeout-error)))
                         :timeout dape-request-timeout))

(defun dape--initialize (conn)
  "Initialize CONN."
  (dape--with-request-bind
      (body error)
      (dape-request conn
                    "initialize"
                    (list :clientID "dape"
                          :adapterID (plist-get (dape--config conn)
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
    (if error
        (progn
          (dape--repl-message (format "Initialize failed due to: %s"
                                      error)
                              'dape-repl-error-face)
          (dape-kill conn))
      (setf (dape--capabilities conn) body)
      ;; See GDB bug 32090
      (unless (plist-get (dape--config conn) 'defer-launch-attach)
        (dape--launch-or-attach conn)))))

(defun dape--launch-or-attach (conn)
  "Launch or attach CONN."
  (dape--with-request-bind
      (_body error)
      (dape-request
       conn
       (or (plist-get (dape--config conn) :request) "launch")
       ;; Transform config to jsonrpc serializable format
       ;; Remove all non `keywordp' keys and transform null to
       ;; :json-false
       (cl-labels
           ((transform-value (value)
              (pcase value
                ('nil :json-false)
                ;; FIXME Need a way to create json null values
                ;;       see #72, :null could be an candidate.
                ;;       Using :null is quite harmless as it has
                ;;       no friction with `dape-configs'
                ;;       evaluation.  So it should be fine to keep
                ;;       supporting it even if it's not the way
                ;;       forwards.
                (:null
                 nil)
                ((pred vectorp)
                 (cl-map 'vector #'transform-value value))
                ((pred listp)
                 (create-body value))
                (_ value)))
            (create-body (config)
              (cl-loop for (key value) on config by 'cddr
                       when (keywordp key)
                       append (list key (transform-value value)))))
         (create-body (dape--config conn))))
    (if error
        (progn (dape--repl-message error 'dape-repl-error-face)
               (dape-kill conn))
      (setf (dape--initialized-p conn) t))))

(defun dape--set-breakpoints-in-buffer (conn buffer &optional cb)
  "Set breakpoints in BUFFER for adapter CONN.
See `dape-request' for expected CB signature."
  (let* ((overlays
          (alist-get buffer
                     (seq-group-by 'overlay-buffer
                                   dape--breakpoints)))
         (lines
          (mapcan (lambda (overlay)
                    (when-let* ((buffer (overlay-buffer overlay))
                                ((buffer-live-p buffer)))
                      (with-current-buffer buffer
                        (list
                         (line-number-at-pos (overlay-start overlay))))))
                  overlays))
         (source
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (or dape--source
                  (list
                   :name (file-name-nondirectory (buffer-file-name buffer))
                   :path (dape--path conn (buffer-file-name buffer) 'remote)))))))
    (dape--with-request-bind
        ((&key breakpoints &allow-other-keys) error)
        (dape-request
         conn
         "setBreakpoints"
         (list
          :source source
          :breakpoints
          (cl-map 'vector
                  (lambda (overlay line)
                    (let (plist it)
                      (setq plist (list :line line))
                      (cond
                       ((setq it (overlay-get overlay :log))
                        (if (dape--capable-p conn :supportsLogPoints)
                            (setq plist (plist-put plist :logMessage it))
                          (dape--repl-message "* Adapter does not support log breakpoints *")))
                       ((setq it (overlay-get overlay :expression))
                        (if (dape--capable-p conn :supportsConditionalBreakpoints)
                            (setq plist (plist-put plist :condition it))
                          (dape--repl-message "* Adapter does not support expression breakpoints *")))
                       ((setq it (overlay-get overlay :hits))
                        (if (dape--capable-p conn :supportsHitConditionalBreakpoints)
                            (setq plist (plist-put plist :hitCondition it))
                          (dape--repl-message "* Adapter does not support hits breakpoints *"))))
                      plist))
                  overlays
                  lines)
          :lines (apply 'vector lines)))
      (cl-loop for breakpoint across breakpoints
               for overlay in overlays
               do (dape--breakpoint-update conn overlay breakpoint))
      (dape--request-return cb error))))

(defun dape--set-exception-breakpoints (conn &optional cb)
  "Set the exception breakpoints for adapter CONN.
The exceptions are derived from `dape--exceptions'.
See `dape-request' for expected CB signature."
  (if (not dape--exceptions)
      (dape--request-return cb)
    (dape-request conn
                  "setExceptionBreakpoints"
                  (list
                   :filters
                   (cl-map 'vector
                           (lambda (exception)
                             (plist-get exception :filter))
                           (seq-filter (lambda (exception)
                                         (plist-get exception :enabled))
                                       dape--exceptions)))
                  cb)))

(defun dape--configure-exceptions (conn &optional cb)
  "Configure exception breakpoints for adapter CONN.
The exceptions are derived from `dape--exceptions'.
See `dape-request' for expected CB signature."
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
                                 (eq (plist-get exception :default) t))))))
                (plist-get (dape--capabilities conn)
                           :exceptionBreakpointFilters)))
  (dape--with-request (dape--set-exception-breakpoints conn)
    (run-hooks 'dape-update-ui-hook)
    (dape--request-return cb)))

(defun dape--set-breakpoints (conn cb)
  "Set breakpoints for adapter CONN.
See `dape-request' for expected CB signature."
  (if-let ((buffers
            (thread-last dape--breakpoints
                         (seq-group-by 'overlay-buffer)
                         (mapcar 'car))))
      (let ((responses 0))
        (dolist (buffer buffers)
          (dape--with-request (dape--set-breakpoints-in-buffer conn buffer)
            (setf responses (1+ responses))
            (when (eq responses (length buffers))
              (dape--request-return cb)))))
    (dape--request-return cb)))

(defun dape--set-data-breakpoints (conn cb)
  "Set data breakpoints for adapter CONN.
See `dape-request' for expected CB signature."
  (if (dape--capable-p conn :supportsDataBreakpoints)
      (dape--with-request-bind
          ((&key breakpoints &allow-other-keys) error)
          (dape-request conn "setDataBreakpoints"
                        (list
                         :breakpoints
                         (cl-loop
                          for plist in dape--data-breakpoints
                          collect (list :dataId (plist-get plist :dataId)
                                        :accessType (plist-get plist :accessType))
                          into breakpoints
                          finally return (apply 'vector breakpoints))))
        (when error
          (message "Failed to setup data breakpoints: %s" error))
        (cl-loop
         for req-breakpoint in dape--data-breakpoints
         for res-breakpoint across (or breakpoints [])
         if (eq (plist-get res-breakpoint :verified) t)
         collect req-breakpoint into verfied-breakpoints else
         collect req-breakpoint into unverfied-breakpoints
         finally do
         (when unverfied-breakpoints
           (dape--repl-message
            (format "Failed setting data breakpoints for %s"
                    (mapconcat (lambda (plist) (plist-get plist :name))
                               unverfied-breakpoints ", "))))
         ;; FIXME Should not remove unverified-breakpoints as they
         ;;       might be verified by another live connection.
         (setq dape--data-breakpoints verfied-breakpoints))
        (dape--request-return cb error))
    (setq dape--data-breakpoints nil)
    (dape--request-return cb)))

(defun dape--update-threads (conn cb)
  "Update threads for CONN in-place if possible.
See `dape-request' for expected CB signature."
  (dape--with-request-bind
      ((&key threads &allow-other-keys) error)
      (dape-request conn "threads" nil)
    (setf (dape--threads conn)
          (mapcar
           (lambda (new-thread)
             (if-let ((old-thread
                       (cl-find-if (lambda (old-thread)
                                     (eql (plist-get new-thread :id)
                                          (plist-get old-thread :id)))
                                   (dape--threads conn))))
                 (plist-put old-thread :name (plist-get new-thread :name))
               new-thread))
           (append threads nil)))
    (dape--request-return cb error)))

(defun dape--stack-trace (conn thread nof cb)
  "Update stack trace in THREAD plist with NOF frames by adapter CONN.
See `dape-request' for expected CB signature."
  (let ((current-nof (length (plist-get thread :stackFrames)))
        (total-frames (plist-get thread :totalFrames))
        (delayed-stack-trace-p
         (dape--capable-p conn :supportsDelayedStackTraceLoading)))
    (cond
     ((or (not (equal (plist-get thread :status) 'stopped))
          (not (integerp (plist-get thread :id)))
          (eql current-nof total-frames)
          (and delayed-stack-trace-p (<= nof current-nof))
          (and (not delayed-stack-trace-p) (> current-nof 0)))
      (dape--request-return cb))
     (t
      (dape--with-request-bind
          ((&key stackFrames totalFrames &allow-other-keys) error)
          (dape-request conn
                        "stackTrace"
                        `(:threadId
                          ,(plist-get thread :id)
                          ,@(when delayed-stack-trace-p
                              (list
                               :startFrame current-nof
                               :levels (- nof current-nof)))
                          ,@(when (and dape-info-stack-buffer-modules
                                       (dape--capable-p conn :supportsValueFormattingOptions))
                              `(:format (:module t)))))
        (cond
         ((not delayed-stack-trace-p)
          (plist-put thread :stackFrames
                     (append stackFrames nil)))
         ;; Sanity check delayed stack trace
         ((length= (plist-get thread :stackFrames) current-nof)
          (plist-put thread :stackFrames
                     (append (plist-get thread :stackFrames)
                             stackFrames
                             nil))))
        (plist-put thread :totalFrames
                   (and (numberp totalFrames) totalFrames))
        (dape--request-return cb error))))))

(defun dape--variables (conn object cb)
  "Update OBJECTs variables by adapter CONN.
See `dape-request' for expected CB signature."
  (let ((variables-reference (plist-get object :variablesReference)))
    (if (or (not (numberp variables-reference))
            (zerop variables-reference)
            (plist-get object :variables)
            (not (jsonrpc-running-p conn)))
        (dape--request-return cb)
      (dape--with-request-bind
          ((&key variables &allow-other-keys) _error)
          (dape-request conn
                        "variables"
                        (list :variablesReference variables-reference))
        (plist-put object
                   :variables
                   (thread-last variables
                                (cl-map 'list 'identity)
                                (seq-filter 'identity)))
        (dape--request-return cb)))))


(defun dape--variables-recursive (conn object path pred cb)
  "Update variables recursivly.
Get variable data from CONN and put result on OBJECT until PRED is nil.
PRED is called with PATH and OBJECT.
See `dape-request' for expected CB signature."
  (let ((objects
         (seq-filter (apply-partially pred path)
                     (or (plist-get object :scopes)
                         (plist-get object :variables)))))
    (if (not objects)
        (dape--request-return cb)
      (let ((responses 0))
        (dolist (object objects)
          (dape--with-request (dape--variables conn object)
            (dape--with-request
                (dape--variables-recursive conn object
                                           (cons (plist-get object :name)
                                                 path)
                                           pred)
              (when (length= objects
                             (setf responses (1+ responses)))
                (dape--request-return cb)))))))))

(defun dape--evaluate-expression (conn frame-id expression context cb)
  "Send evaluate request to adapter CONN.
FRAME-ID specifies which frame the EXPRESSION is evaluated in and
CONTEXT which the result is going to be displayed in.
See `dape-request' for expected CB signature."
  (dape-request conn "evaluate"
                (append (when (dape--stopped-threads conn)
                          (list :frameId frame-id))
                        (list :expression expression
                              :context context))
                cb))

(defun dape--set-variable (conn variable-reference variable value)
  "Set VARIABLE to VALUE with VARIABLE-REFERENCE in for CONN.
Calls setVariable endpoint if VARIABLE-REFERENCE is an number and
setExpression if it's not.
Runs the appropriate hooks on non error response."
  (cond
   ;; `variable' from an "variable" request
   ((and (dape--capable-p conn :supportsSetVariable)
         (numberp variable-reference))
    (dape--with-request-bind
        (body error)
        (dape-request conn
                      "setVariable"
                      (list
                       :variablesReference variable-reference
                       :name (plist-get variable :name)
                       :value value))
      (if error
          (message "%s" error)
        ;; Would make more sense to update all variables after
        ;; setVariable request but certain adapters cache "variable"
        ;; response so we just update the variable in question in
        ;; place.
        (plist-put variable :variables nil)
        (cl-loop for (key value) on body by 'cddr
                 do (plist-put variable key value))
        (run-hooks 'dape-update-ui-hook))))
   ;; `variable' from an "evaluate" request
   ((and (dape--capable-p conn :supportsSetExpression)
         (or (plist-get variable :evaluateName)
             (plist-get variable :name)))
    (dape--with-request-bind
        (_body error)
        (dape-request conn
                      "setExpression"
                      (list :frameId (plist-get (dape--current-stack-frame conn) :id)
                            :expression (or (plist-get variable :evaluateName)
                                            (plist-get variable :name))
                            :value value))
      (if error
          (message "%s" error)
        ;; Update all variables
        (dape--update conn 'variables nil))))
   ((user-error "Unable to set variable"))))

(defun dape--scopes (conn stack-frame cb)
  "Send scopes request to CONN for STACK-FRAME plist.
See `dape-request' for expected CB signature."
  (if-let ((id (plist-get stack-frame :id))
           ((not (plist-get stack-frame :scopes))))
      (dape--with-request-bind
          ((&key scopes &allow-other-keys) error)
          (dape-request conn "scopes" (list :frameId id))
        (plist-put stack-frame :scopes (append scopes nil))
        (dape--request-return cb error))
    (dape--request-return cb)))

(defun dape--update (conn clear display)
  "Update adapter CONN data and UI.
CLEAR can be one of:
 `stack-frames': clear stack data for each thread.  This effects
current selected stack frame.
 `variables': keep stack frame data but clear variables for each
frame.
 nil: keep all available data.
If DISPLAY is non nil display buffer containing source of current
selected stack frame."
  (let ((current-thread (dape--current-thread conn)))
    (dolist (thread (dape--threads conn))
      (pcase clear
        ('stack-frames
         (plist-put thread :stackFrames nil)
         (plist-put thread :totalFrames nil))
        ('variables
         (dolist (frame (plist-get thread :stackFrames))
           (plist-put frame :scopes nil)))))
    (dape--with-request (dape--stack-trace conn current-thread 1)
      (when display
        (dape--stack-frame-display conn))
      (dape--with-request (dape--scopes conn (dape--current-stack-frame conn))
        (run-hooks 'dape-update-ui-hook)))))


;;; Incoming requests

(cl-defgeneric dape-handle-request (_conn _command _arguments)
  "Sink for all unsupported requests." nil)

(define-derived-mode dape-shell-mode shell-mode "Shell"
  "Major mode for interacting with an debugged program.")

(cl-defmethod dape-handle-request (conn (_command (eql runInTerminal)) arguments)
  "Handle runInTerminal requests.
Starts a new adapter CONNs from ARGUMENTS."
  (let ((default-directory
         (or (when-let ((cwd (plist-get arguments :cwd)))
               (dape--path conn cwd 'local))
             default-directory))
        (process-environment
         (or (cl-loop for (key value) on (plist-get arguments :env) by 'cddr
                      collect
                      (format "%s=%s"
                              (substring (format "%s" key) 1)
                              value))
             process-environment))
        (buffer (get-buffer-create "*dape-shell*")))
    (with-current-buffer buffer
      (dape-shell-mode)
      (shell-command-save-pos-or-erase))
    (let ((process
           (make-process :name "dape shell"
                         :buffer buffer
                         :command (append (plist-get arguments :args) nil)
                         :filter 'comint-output-filter
                         :sentinel 'shell-command-sentinel
                         :file-handler t)))
      (dape--display-buffer buffer)
      (list :processId (process-id process)))))

(cl-defmethod dape-handle-request (conn (_command (eql startDebugging)) arguments)
  "Handle adapter CONNs startDebugging requests with ARGUMENTS.
Starts a new adapter connection as per request of the debug adapter."
  (let ((config (plist-get arguments :configuration))
        (request (plist-get arguments :request)))
    (cl-loop with socket-conn-p = (plist-get (dape--config conn) 'port)
             for (key value) on (dape--config conn) by 'cddr
             unless (or (keywordp key)
                        (and socket-conn-p (eq key 'command)))
             do (plist-put config key value))
    (when request
      (plist-put config :request request))
    (let ((new-connection
           (dape--create-connection config (or (dape--parent conn)
                                               conn))))
      (push new-connection (dape--children conn))
      (dape--start-debugging new-connection)))
  nil)


;;; Events

(cl-defgeneric dape-handle-event (_conn _event _body)
  "Sink for all unsupported events." nil)

(cl-defmethod dape-handle-event (conn (_event (eql initialized)) _body)
  "Handle adapter CONNs initialized events."
  (dape--update-state conn 'initialized)
  (dape--with-request (dape--configure-exceptions conn)
    (dape--with-request (dape--set-breakpoints conn)
      (dape--with-request (dape--set-data-breakpoints conn)
        (dape--with-request (dape-request conn "configurationDone" nil)
          ;; See GDB bug 32090
          (when (plist-get (dape--config conn) 'defer-launch-attach)
            (dape--launch-or-attach conn)))))))

(cl-defmethod dape-handle-event (conn (_event (eql capabilities)) body)
  "Handle adapter CONNs capabilities events.
BODY is an plist of adapter capabilities."
  (setf (dape--capabilities conn) (plist-get body :capabilities))
  (dape--configure-exceptions conn))

(cl-defmethod dape-handle-event (conn (_event (eql breakpoint)) body)
  "Handle adapter CONNs breakpoint events.
Update `dape--breakpoints' according to BODY."
  (when-let* ((breakpoint (plist-get body :breakpoint))
              (id (plist-get breakpoint :id))
              (overlay (seq-find (lambda (ov)
                                   (equal (overlay-get ov 'dape-id) id))
                                 dape--breakpoints)))
    (dape--breakpoint-update conn overlay breakpoint)))

(cl-defmethod dape-handle-event (conn (_event (eql module)) body)
  "Handle adapter CONNs module events.
Stores `dape--modules' from BODY."
  (let ((reason (plist-get body :reason))
        (id (thread-first body (plist-get :module) (plist-get :id))))
    (pcase reason
      ("new"
       (push (plist-get body :module) (dape--modules conn)))
      ("changed"
       (cl-loop with plist = (cl-find id (dape--modules conn)
                                      :key (lambda (module)
                                             (plist-get module :id)))
                for (key value) on body by 'cddr
                do (plist-put plist key value)))
      ("removed"
       (cl-delete id (dape--modules conn)
                  :key (lambda (module) (plist-get module :id)))))))

(cl-defmethod dape-handle-event (conn (_event (eql loadedSource)) body)
  "Handle adapter CONNs loadedSource events.
Stores `dape--sources' from BODY."
  (let ((reason (plist-get body :reason))
        (id (thread-first body (plist-get :source) (plist-get :id))))
    (pcase reason
      ("new"
       (push (plist-get body :source) (dape--sources conn)))
      ("changed"
       (cl-loop with plist = (cl-find id (dape--sources conn)
                                      :key (lambda (source)
                                             (plist-get source :id)))
                for (key value) on body by 'cddr
                do (plist-put plist key value)))
      ("removed"
       (cl-delete id (dape--sources conn)
                  :key (lambda (source) (plist-get source :id)))))))

(cl-defmethod dape-handle-event (conn (_event (eql process)) body)
  "Handle adapter CONNs process events.
Logs and sets state based on BODY contents."
  (let ((start-method
         (format "%sed" (or (plist-get body :startMethod)
                            "start"))))
    (dape--update-state conn (intern start-method))
    (dape--repl-message
     (format "Process %s %s" start-method (plist-get body :name)))))

(cl-defmethod dape-handle-event (conn (_event (eql thread)) body)
  "Handle adapter CONNs thread events.
Stores `dape--thread-id' and updates/adds thread in
`dape--thread' from BODY."
  (cl-destructuring-bind (&key threadId reason &allow-other-keys)
      body
    (when (equal reason "started")
      ;; For adapters that does not send an continued request use
      ;; thread started as an way to switch from `initialized' to
      ;; running.
      (dape--update-state conn 'running)
      (dape--maybe-select-thread conn (plist-get body :threadId) nil))
    (let ((update-handle
           ;; Need to store handle before threads request to guard
           ;; against an overwriting thread status if event is firing
           ;; while threads request is in flight
           (dape--threads-make-update-handle conn)))
      (dape--with-request (dape--update-threads conn)
        (dape--threads-set-status conn threadId nil
                                  (if (equal reason "exited")
                                      'exited
                                    'running)
                                  update-handle)
        (run-hooks 'dape-update-ui-hook)))))

(cl-defmethod dape-handle-event (conn (_event (eql stopped)) body)
  "Handle adapter CONNs stopped events.
Sets `dape--thread-id' from BODY and invokes ui refresh with
`dape--update'."
  (cl-destructuring-bind
      (&key threadId reason allThreadsStopped hitBreakpointIds
            &allow-other-keys)
      body
    (dape--update-state conn 'stopped reason)
    (dape--maybe-select-thread conn threadId 'force)
    ;; Reset stack id to force a new frame in
    ;; `dape--current-stack-frame'.
    (setf (dape--stack-id conn) nil
          ;; Reset exception description
          (dape--exception-description conn) nil)
    ;; Important to do this before `dape--update' to be able to setup
    ;; breakpoints description.
    (when (equal reason "exception")
      ;; Output exception info in overlay and REPL
      (let* ((texts
              (seq-filter 'stringp
                          (list (plist-get body :text)
                                (plist-get body :description))))
             (str (mapconcat 'identity texts ":\n\t")))
        (setf (dape--exception-description conn) str)
        (dape--repl-message str 'dape-repl-error-face)))
    ;; Update breakpoints hits
    (dape--breakpoints-stopped hitBreakpointIds)
    ;; Update `dape--threads'
    (let ((update-handle
           ;; Need to store handle before threads request to guard
           ;; against an overwriting thread status if event is firing
           ;; while threads request is in flight
           (dape--threads-make-update-handle conn)))
      (dape--with-request (dape--update-threads conn)
        (dape--threads-set-status conn threadId (eq allThreadsStopped t)
                                  'stopped update-handle)
        (dape--update conn 'stack-frames t)))
    (run-hooks 'dape-stopped-hook)))

(cl-defmethod dape-handle-event (conn (_event (eql continued)) body)
  "Handle adapter CONN continued events.
Sets `dape--thread-id' from BODY if not set."
  (cl-destructuring-bind
      (&key threadId (allThreadsContinued t) &allow-other-keys)
      body
    (dape--update-state conn 'running)
    (dape--stack-frame-cleanup)
    (dape--maybe-select-thread conn threadId nil)
    (dape--threads-set-status conn threadId (eq allThreadsContinued t) 'running
                              (dape--threads-make-update-handle conn))
    (run-hooks 'dape-update-ui-hook)))

(cl-defmethod dape-handle-event (_conn (_event (eql output)) body)
  "Handle output events by printing BODY with `dape--repl-message'."
  (pcase (plist-get body :category)
    ((or "stdout" "console" "output")
     (dape--repl-message (plist-get body :output)))
    ("stderr"
     (dape--repl-message (plist-get body :output) 'dape-repl-error-face))))

(cl-defmethod dape-handle-event (conn (_event (eql exited)) body)
  "Handle adapter CONNs exited events.
Prints exit code from BODY."
  (dape--update-state conn 'exited)
  (dape--stack-frame-cleanup)
  (dape--repl-message
   (format "* Exit code: %d *" (plist-get body :exitCode))
   (if (zerop (plist-get body :exitCode))
       'dape-repl-success-face
     'dape-repl-error-face)))

(cl-defmethod dape-handle-event (conn (_event (eql terminated)) _body)
  "Handle adapter CONNs terminated events.
Killing the adapter and it's CONN."
  (let ((child-conn-p (dape--parent conn)))
    (dape--with-request (dape-kill conn)
      (when (not child-conn-p)
        ;; HACK remove dubble terminated print for dlv
        (unless (eq (dape--state conn) 'terminated)
          (dape--repl-message "* Session terminated *"))
        (dape--update-state conn 'terminated)))))


;;; Startup/Setup

(defun dape--start-debugging (conn)
  "Preform some cleanup and start debugging with CONN."
  (unless (dape--parent conn)
    (dape--stack-frame-cleanup)
    (dape--breakpoints-reset)
    (cl-loop for (_ buffer) on dape--source-buffers by 'cddr
             when (buffer-live-p buffer)
             do (kill-buffer buffer))
    (setq dape--source-buffers nil
          dape--repl-insert-text-guard nil)
    (unless dape-active-mode
      (dape-active-mode +1))
    (dape--update-state conn 'starting)
    (run-hooks 'dape-update-ui-hook))
  (dape--initialize conn))

(defun dape--create-connection (config &optional parent)
  "Create symbol `dape-connection' instance from CONFIG.
If started by an startDebugging request expects PARENT to
symbol `dape-connection'."
  (unless (plist-get config 'command-cwd)
    (plist-put config 'command-cwd default-directory))
  (let ((default-directory (plist-get config 'command-cwd))
        (retries 30)
        process server-process)
    (cond
     ;; socket conn
     ((plist-get config 'port)
      ;; start server
      (when (plist-get config 'command)
        (let ((stderr-buffer
               (get-buffer-create "*dape-server stderr*"))
              (command
               (cons (plist-get config 'command)
                     (cl-map 'list 'identity
                             (plist-get config 'command-args)))))
          (setq server-process
                (make-process :name "dape adapter"
                              :command command
                              :filter (lambda (_process string)
                                        (dape--repl-message string))
                              :noquery t
                              :file-handler t
                              :stderr stderr-buffer))
          (process-put server-process 'stderr-buffer stderr-buffer)
          (when dape-debug
            (dape--repl-message (format "* Adapter server started with %S *"
                                        (mapconcat 'identity
                                                   command " ")))))
        ;; FIXME Why do I need this?
        (when (file-remote-p default-directory)
          (sleep-for 0.300)))
      ;; connect to server
      (let ((host (or (plist-get config 'host) "localhost")))
        (while (and (not process)
                    (> retries 0))
          (ignore-errors
            (setq process
                  (make-network-process :name
                                        (format "dape adapter%s connection"
                                                (if parent " child" ""))
                                        :host host
                                        :coding 'utf-8-emacs-unix
                                        :service (plist-get config 'port)
                                        :noquery t)))
          (sleep-for 0.100)
          (setq retries (1- retries)))
        (if (zerop retries)
            (progn
              (dape--repl-message (format "* Unable to connect to dap server at %s:%d *"
                                          host (plist-get config 'port))
                                  'dape-repl-error-face)
              (dape--repl-message
               (format "* Connection is configurable by %s and %s keys *"
                       (propertize "host" 'font-lock-face 'font-lock-keyword-face)
                       (propertize "port" 'font-lock-face 'font-lock-keyword-face)))
              ;; barf server std-err
              (when-let ((buffer
                          (and server-process
                               (process-get server-process 'stderr-buffer))))
                (with-current-buffer buffer
                  (dape--repl-message (buffer-string) 'dape-repl-error-face)))
              (delete-process server-process)
              (user-error "Unable to connect to server"))
          (when dape-debug
            (dape--repl-message
             (format "* %s to adapter established at %s:%s *"
                     (if parent "Child connection" "Connection")
                     host (plist-get config 'port)))))))
     ;; stdio conn
     (t
      (let ((command
             (cons (plist-get config 'command)
                   (cl-map 'list 'identity
                           (plist-get config 'command-args)))))
        (setq process
              (make-process :name "dape adapter"
                            :command command
                            :connection-type 'pipe
                            :coding 'utf-8-emacs-unix
                            :noquery t
                            :file-handler t))
        (when dape-debug
          (dape--repl-message (format "* Adapter started with %S *"
                                      (mapconcat 'identity command " ")))))))
    (make-instance
     'dape-connection
     :name "dape-connection"
     :config config
     :parent parent
     :server-process server-process
     :events-buffer-config `(:size ,(if dape-debug nil 0)
                                   :format full)
     :on-shutdown
     (lambda (conn)
       ;; error prints
       (unless (dape--initialized-p conn)
         (dape--repl-message
          (concat "* Adapter "
                  (when (dape--parent conn)
                    "child ")
                  "connection shutdown without successfully initializing *")
          'dape-repl-error-face)
         ;; barf config
         (dape--repl-message
          (format "Configuration:\n%s"
                  (cl-loop for (key value) on (dape--config conn) by 'cddr
                           concat (format "  %s %S\n" key value)))
          'dape-repl-error-face)
         ;; barf connection stdout
         (when-let* ((proc (jsonrpc--process conn))
                     (buffer (process-buffer proc))
                     ((buffer-live-p buffer)))
           (dape--repl-message (with-current-buffer buffer (buffer-string))))
         ;; barf connection stderr
         (when-let* ((proc (jsonrpc--process conn))
                     (buffer (process-get proc 'jsonrpc-stderr))
                     ((buffer-live-p buffer))
                     (stderr (with-current-buffer buffer (buffer-string))))
           (dape--repl-message stderr 'dape-repl-error-face))
         ;; barf server stderr
         (when-let* ((server-proc (dape--server-process conn))
                     (buffer (process-get server-proc 'stderr-buffer)))
           (with-current-buffer buffer
             (dape--repl-message (buffer-string) 'dape-repl-error-face))))
       ;; cleanup server process
       (unless (dape--parent conn)
         (dape--stack-frame-cleanup)
         (when-let ((server-process
                     (dape--server-process conn)))
           (delete-process server-process)
           (while (process-live-p server-process)
             (accept-process-output nil nil 0.1))))
       ;; ui
       (when (eq dape--connection conn)
         (dape-active-mode -1)
         (force-mode-line-update t)))
     :request-dispatcher 'dape-handle-request
     :notification-dispatcher 'dape-handle-event
     :process process)))


;;; Commands

(defun dape-next (conn)
  "Step one line (skip functions)
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn "next"))

(defun dape-step-in (conn)
  "Step into function/method.  If not possible behaves like `dape-next'.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn "stepIn"))

(defun dape-step-out (conn)
  "Step out of function/method.  If not possible behaves like `dape-next'.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn "stepOut"))

(defun dape-continue (conn)
  "Resumes execution.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (unless (dape--stopped-threads conn)
    (user-error "No stopped threads"))
  (let ((body (dape--thread-id-object conn)))
    (unless body
      (user-error "Unable to derive thread to continued"))
    (dape--with-request-bind
        ((&key (allThreadsContinued t) &allow-other-keys) error)
        (dape-request conn "continue" body)
      (if error
          (error "Failed to continue: %s" error)
        ;; From specification [continued] event:
        ;; A debug adapter is not expected to send this event in
        ;; response to a request that implies that execution
        ;; continues, e.g. launch or continue.
        (dape-handle-event
         conn 'continued
         `(,@body :allThreadsContinued ,allThreadsContinued))))))

(defun dape-pause (conn)
  "Pause execution.
CONN is inferred for interactive invocations."
  (interactive (list (or (dape--live-connection 'running t)
                         (dape--live-connection 'parent))))
  (when (dape--stopped-threads conn)
    ;; cpptools crashes on pausing an paused thread
    (user-error "Thread already is stopped"))
  (dape--with-request-bind
      (_body error)
      (dape-request conn "pause" (dape--thread-id-object conn))
    (when error
      (error "Failed to pause: %s" error))))

(defun dape-restart (&optional conn)
  "Restart debugging session.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'last t)))
  (dape--stack-frame-cleanup)
  (dape--breakpoints-reset)
  (cond
   ((and conn
         (dape--capable-p conn :supportsRestartRequest))
    (setq dape--connection-selected nil)
    (setf (dape--threads conn) nil)
    (setf (dape--thread-id conn) nil)
    (setf (dape--modules conn) nil)
    (setf (dape--sources conn) nil)
    (setf (dape--restart-in-progress-p conn) t)
    (dape--with-request (dape-request conn "restart" nil)
      (setf (dape--restart-in-progress-p conn) nil)))
   (dape-history
    (dape (apply 'dape--config-eval (dape--config-from-string (car dape-history)))))
   ((user-error "Unable to derive session to restart, run `dape'"))))

(defun dape-kill (conn &optional cb with-disconnect)
  "Kill debug session.
CB will be called after adapter termination.  With WITH-DISCONNECT use
disconnect instead of terminate used internally as a fallback to
terminate.  CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'parent)))
  (cond
   ((and conn (jsonrpc-running-p conn)
         (not with-disconnect)
         (dape--capable-p conn :supportsTerminateRequest))
    (dape--with-request-bind (_body error)
        (dape-request conn "terminate" nil)
      ;; We have to give up trying to kill the debuggee in an correct
      ;; way if the request timeout, otherwise we might force the
      ;; user to kill the process in some other way.
      (if (and error (not (eq error dape--timeout-error)))
          (dape-kill cb 'with-disconnect)
        (jsonrpc-shutdown conn)
        (dape--request-return cb))))
   ((and conn (jsonrpc-running-p conn))
    (dape--with-request
        (dape-request conn "disconnect"
                      `(:restart
                        :json-false
                        ,@(when (dape--capable-p conn :supportTerminateDebuggee)
                            (list :terminateDebuggee t))))
      (jsonrpc-shutdown conn)
      (dape--request-return cb)))
   (t
    (dape--request-return cb))))

(defun dape-disconnect-quit (conn)
  "Kill adapter but try to keep debuggee live.
This will leave a decoupled debugged process with no debugge
connection.  CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'parent)))
  (dape--kill-buffers 'skip-process-buffers)
  (dape--with-request
      (dape-request conn "disconnect"
                    (list :terminateDebuggee nil))
    (jsonrpc-shutdown conn)
    (dape--kill-buffers)))

(defun dape-quit (&optional conn)
  "Kill debug session and kill related dape buffers.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'parent t)))
  (dape--kill-buffers 'skip-process-buffers)
  (if (not conn)
      (dape--kill-buffers)
    (let (;; Use a lower timeout, if trying to kill an to kill an
          ;; unresponsive adapter 10s is an long time to wait.
          (dape-request-timeout 3))
      (dape--with-request (dape-kill conn)
        (dape--kill-buffers)))))

(defun dape-breakpoint-toggle ()
  "Add or remove breakpoint at current line."
  (interactive)
  (cond
   ((not (seq-filter (lambda (ov)
                       (overlay-get ov :breakpoint))
                     (dape--breakpoints-at-point)))
    (dape--breakpoint-place))
   (t
    (dape-breakpoint-remove-at-point))))

(defun dape-breakpoint-log (log-message)
  "Add log breakpoint at line.
Argument LOG-MESSAGE contains string to print to *dape-repl*.
Expressions within `{}` are interpolated."
  (interactive
   (list
    (read-string "Log (Expressions within `{}` are interpolated): "
                 (when-let ((prev-log-breakpoint
                             (seq-find (lambda (ov)
                                         (overlay-get ov :log))
                                       (dape--breakpoints-at-point))))
                   (overlay-get prev-log-breakpoint :log)))))
  (cond
   ((string-empty-p log-message)
    (dape-breakpoint-remove-at-point))
   (t
    (dape--breakpoint-place :log log-message))))

(defun dape-breakpoint-expression (expr-message)
  "Add expression breakpoint at current line.
When EXPR-MESSAGE is evaluated as true threads will pause at current line."
  (interactive
   (list
    (read-string "Condition: "
                 (when-let ((prev-expr-breakpoint
                             (seq-find (lambda (ov)
                                         (overlay-get ov :expression))
                                       (dape--breakpoints-at-point))))
                   (overlay-get prev-expr-breakpoint :expression)))))
  (cond
   ((string-empty-p expr-message)
    (dape-breakpoint-remove-at-point))
   (t
    (dape--breakpoint-place :expression expr-message))))

(defun dape-breakpoint-hits (hits)
  "Add hits breakpoint at line.
An hit HITS is an string matching regex:
\"\\(!=\\|==\\|[%<>]\\) [:digit:]\"

When HITS-EXPRESSION is evaluated as true threads will pause at current line."
  (interactive
   (list
    (pcase-let ((`(_ ,operator)
                 (let (use-dialog-box)
                   (read-multiple-choice
                    "Operator"
                    '((?= "==" "Equals") (?! "!=" "Not equals")
                      (?< "<" "Less then") (?> ">" "Greater then")
                      (?% "%" "Modulus"))))))
      (thread-last operator
                   (format "Breakpoint hit condition %s ")
                   (read-number)
                   (format "%s %d" operator)))))
  (cond
   ((string-empty-p hits)
    (dape-breakpoint-remove-at-point))
   (t
    (dape--breakpoint-place :hits hits))))

(defun dape-breakpoint-remove-at-point (&optional skip-update)
  "Remove breakpoint, log breakpoint and expression at current line.
When SKIP-UPDATE is non nil, does not notify adapter about removal."
  (interactive)
  (dolist (breakpoint (dape--breakpoints-at-point))
    (dape--breakpoint-remove breakpoint skip-update)))

(defun dape-breakpoint-remove-all ()
  "Remove all breakpoints."
  (interactive)
  (let ((buffers-breakpoints
         (seq-group-by 'overlay-buffer dape--breakpoints)))
    (pcase-dolist (`(,buffer . ,breakpoints) buffers-breakpoints)
      (dolist (breakpoint breakpoints)
        (dape--breakpoint-remove breakpoint t))
      (dolist (conn (dape--live-connections))
        (dape--set-breakpoints-in-buffer conn buffer)))))

(defun dape-select-thread (conn thread-id)
  "Select current thread for adapter CONN by THREAD-ID."
  (interactive
   (let* ((conn (dape--live-connection 'last))
          (collection
           (cl-loop
            with conns = (dape--live-connections)
            with conn-prefix-p = (length>
                                  (cl-remove-if-not 'dape--threads conns) 1)
            for conn in conns
            for index upfrom 1
            append (cl-loop
                    for thread in (dape--threads conn)
                    for name = (concat
                                (when conn-prefix-p
                                  (format "%s: " index))
                                (format "%s %s"
                                        (plist-get thread :id)
                                        (plist-get thread :name)))
                    collect (list name conn (plist-get thread :id)))))
          (thread-name
           (completing-read
            (format "Select thread (current %s): "
                    ;; TODO Show current thread with connection prefix
                    (thread-first conn (dape--current-thread)
                                  (plist-get :name)))
            collection
            nil t)))
     (alist-get thread-name collection nil nil 'equal)))
  (setf (dape--thread-id conn) thread-id)
  (setq dape--connection-selected conn)
  (dape--update conn nil t)
  (dape--mode-line-format))

(defun dape-select-stack (conn stack-id)
  "Selected current stack for adapter CONN by STACK-ID."
  (interactive
   (let* ((conn (dape--live-connection 'stopped))
          (current-thread (dape--current-thread conn))
          (collection
           (let (done)
             (dape--with-request
                 (dape--stack-trace conn current-thread dape-stack-trace-levels)
               ;; Only one stack frame is guaranteed to be available,
               ;; so we need to reach out to make sure we got the full set.
               ;; See `dape--stack-trace'.
               (setf done t))
             (with-timeout (5 nil)
               (while (not done) (accept-process-output nil 0.1)))
             (mapcar (lambda (stack) (cons (plist-get stack :name)
                                           (plist-get stack :id)))
                     (plist-get current-thread :stackFrames))))
          (stack-name
           (completing-read (format "Select stack (current %s): "
                                    (thread-first conn
                                                  (dape--current-stack-frame)
                                                  (plist-get :name)))
                            collection
                            nil t)))
     (list conn (alist-get stack-name collection nil nil 'equal))))
  (setf (dape--stack-id conn) stack-id)
  (dape--update conn nil t))

(defun dape-stack-select-up (conn n)
  "Select N stacks above current selected stack for adapter CONN."
  (interactive (list (dape--live-connection 'stopped) 1))
  (if (dape--stopped-threads conn)
      (let* ((current-stack (dape--current-stack-frame conn))
             (stacks (plist-get (dape--current-thread conn) :stackFrames))
             (i (cl-loop for i upfrom 0
                         for stack in stacks
                         when (equal stack current-stack)
                         return (+ i n))))
        (if (not (and (<= 0 i) (< i (length stacks))))
            (message "Index %s out of range" i)
          (dape-select-stack conn (plist-get (nth i stacks) :id))))
    (message "No stopped threads")))

(defun dape-stack-select-down (conn n)
  "Select N stacks below current selected stack for adapter CONN."
  (interactive (list (dape--live-connection 'stopped) 1))
  (dape-stack-select-up conn (* n -1)))

(defun dape-watch-dwim (expression &optional skip-add skip-remove)
  "Add or remove watch for EXPRESSION.
Watched symbols are displayed in *`dape-info' Watch* buffer.
*`dape-info' Watch* buffer is displayed by executing the `dape-info'
command.
Optional argument SKIP-ADD limits usage to only removal of watched vars.
Optional argument SKIP-REMOVE limits usage to only adding watched vars."
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
      ;; FIXME Remove dependency on ui in core commands
      (dape--display-buffer (dape--info-get-buffer-create 'dape-info-watch-mode))))
  (run-hooks 'dape-update-ui-hook))

(defun dape-evaluate-expression (conn expression)
  "Evaluate EXPRESSION, if region is active evaluate region.
EXPRESSION can be an expression or adapter command, as it's evaluated in
repl context.  CONN is inferred for interactive invocations."
  (interactive
   (list
    (or (dape--live-connection 'stopped t)
        (dape--live-connection 'last))
    (if (region-active-p)
        (buffer-substring (region-beginning)
                          (region-end))
      (read-string "Evaluate: "
                   (thing-at-point 'symbol)))))
  (let ((interactive-p (called-interactively-p 'any)))
    (dape--with-request-bind
        ((&key result &allow-other-keys) error)
        (dape--evaluate-expression conn
                                   (plist-get (dape--current-stack-frame conn) :id)
                                   (substring-no-properties expression)
                                   "repl")
      (when interactive-p
        (if error
            (message "Evaluation failed %s" error)
          (message "%s" (or (and (stringp result)
                                 (not (string-empty-p result))
                                 result)
                            "Evaluation done")))))))

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
  (dape--with-request (dape-kill (dape--live-connection 'parent t))
    (dape--config-ensure config t)
    ;; Hooks need to be run before any repl messaging but after we
    ;; have ensured that config is executable.
    (run-hooks 'dape-start-hook)
    (when-let ((fn (or (plist-get config 'fn) 'identity))
               (fns (or (and (functionp fn) (list fn))
                        (and (listp fn) fn))))
      (setq config
            (seq-reduce (lambda (config fn) (funcall fn config))
                        (append fns dape-default-config-functions)
                        (copy-tree config))))
    (if (and (not skip-compile) (plist-get config 'compile))
        (dape--compile config)
      (setq dape--connection
            (dape--create-connection config))
      (dape--start-debugging dape--connection))))


;;; Compile

(defvar-local dape--compile-config nil)

(defun dape--compile-compilation-finish (buffer str)
  "Hook for `dape--compile-compilation-finish'.
Using BUFFER and STR."
  (remove-hook 'compilation-finish-functions
               #'dape--compile-compilation-finish)
  (cond
   ((equal "finished\n" str)
    (dape dape--compile-config 'skip-compile)
    (run-hook-with-args 'dape-compile-hook buffer))
   (t
    (dape--repl-message (format "* Compilation failed %s *"
                                (string-trim-right str))))))

(defun dape--compile (config)
  "Start compilation for CONFIG."
  (let ((default-directory (dape--guess-root config))
        (command (plist-get config 'compile)))
    (funcall dape-compile-fn command)
    (with-current-buffer (compilation-find-buffer)
      (setq dape--compile-config config)
      (add-hook 'compilation-finish-functions
                #'dape--compile-compilation-finish nil t))))


;;; Memory viewer

(defvar-local dape--memory-address nil
  "Buffer local var to keep track of current address.")

(defvar dape--memory-debounce-timer (timer-create)
  "Debounce context for `dape-memory-revert'.")

(defun dape--memory-address-number ()
  "Return `dape--memory-address' as an number."
  (thread-first dape--memory-address (substring 2) (string-to-number 16)))

(defun dape--memory-revert (&optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for `dape-memory-mode'."
  (let* ((conn (dape--live-connection 'last))
         (write-capable-p (dape--capable-p conn :supportsWriteMemoryRequest)))
    (unless (dape--capable-p conn :supportsReadMemoryRequest)
      (user-error "Adapter not capable of reading memory"))
    (unless dape--memory-address
      (user-error "`dape--memory-address' not set"))
    (dape--with-request-bind
        ((&key address data &allow-other-keys) error)
        (dape-request conn "readMemory"
                      (list :memoryReference dape--memory-address
                            :count dape-memory-page-size))
      (cond
       (error (message "Failed to read memory: %s" error))
       ((not data) (message "No bytes returned from adapter"))
       (t
        (setq dape--memory-address address
              buffer-undo-list nil)
        (let ((inhibit-read-only t)
              (temp-buffer (generate-new-buffer " *temp*" t))
              (address (dape--memory-address-number))
              (buffer-empty-p (zerop (buffer-size))))
          (with-current-buffer temp-buffer
            (insert (base64-decode-string data))
            (let (buffer-undo-list)
              (hexlify-buffer))
            ;; Now we need to translate the address fields after the
            ;; fact ugghh
            (goto-char (point-min))
            (while (re-search-forward "^[0-9a-f]+" nil t)
              (let ((address
                     (format "%08x" (+ address
                                       (string-to-number (match-string 0) 16)))))
                (delete-region (match-beginning 0) (match-end 0))
                ;; `hexl' does not support address over 8 hex chars
                (insert (append (substring address (- (length address) 8)))))))
          (replace-buffer-contents temp-buffer)
          (when buffer-empty-p
            (goto-char (point-min)))
          (kill-buffer temp-buffer))
        (set-buffer-modified-p nil)
        (when write-capable-p
	  (add-hook 'write-contents-functions #'dape--memory-write))
        (rename-buffer (format "*dape-memory @ %s*" address) t))))))

(defun dape--memory-write ()
  "Write buffer contents to stopped connection."
  (let ((conn (dape--live-connection 'last))
        (buffer (current-buffer))
        (start (point-min))
        (end (point-max))
        (address dape--memory-address))
    (with-temp-buffer
      (insert-buffer-substring buffer start end)
      (dehexlify-buffer)
      (dape--with-request-bind
          (_body error)
          (dape-request conn "writeMemory"
                        (list :memoryReference address
                              :data (base64-encode-string (buffer-string) t)))
        (if error
            (message "Failed to write memory: %s" error)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (message "Memory written successfully at %s" address)
          (dape--update conn 'variables nil)))))
  ;; Return `t' to signal buffer written
  t)

(defun dape--memory-print-current-point-info (&rest _ignored)
  "Print address at point."
  (let ((addr (+ (hexl-current-address) (dape--memory-address-number))))
    (format "Current memory address is %d/0x%08x" addr addr)))

(define-derived-mode dape-memory-mode hexl-mode "Memory"
  "Mode for reading and writing memory."
  :interactive nil
  ;; TODO Look for alternatives to hexl, which handles address offsets
  (add-hook 'eldoc-documentation-functions
            #'dape--memory-print-current-point-info nil t)
  ;; FIXME Is `revert-buffer-in-progress-p' is not respected
  ;;       as most of the work is done in an callback.
  (setq revert-buffer-function #'dape--memory-revert))

(define-key dape-memory-mode-map "\C-x]" #'dape-memory-next-page)
(define-key dape-memory-mode-map "\C-x[" #'dape-memory-previous-page)

(defun dape-memory-next-page (&optional backward)
  "Move address `dape-memory-page-size' forward.
When BACKWARD is non nil move backward instead."
  (interactive nil dape-memory-mode)
  (let ((op (if backward '- '+)))
    (dape-read-memory
     (format "0x%08x"
             (funcall op
                      (dape--memory-address-number)
                      dape-memory-page-size))
     t)))

(defun dape-memory-previous-page ()
  "Move address `dape-memory-page-size' backward."
  (interactive nil dape-memory-mode)
  (dape-memory-next-page 'backward))

(defun dape-memory-revert ()
  "Revert all `dape-memory-mode' buffers."
  (dape--with-debounce dape--memory-debounce-timer dape-ui-debounce-time
    (cl-loop for buffer in (buffer-list)
             when (eq (with-current-buffer buffer major-mode)
                      'dape-memory-mode)
             do (with-current-buffer buffer (revert-buffer)))))

(defun dape-read-memory (address &optional reuse-buffer)
  "Read `dape-memory-page-size' bytes of memory at ADDRESS.
If REUSE-BUFFER is non nil reuse the current buffer to display result
of memory read."
  (interactive
   (list (string-trim
          (read-string "Address: "
                       (when-let ((number (thing-at-point 'number)))
                         (format "0x%08x" number))))))
  (let ((conn (dape--live-connection 'stopped)))
    (unless (dape--capable-p conn :supportsReadMemoryRequest)
      (user-error "Adapter not capable of reading memory"))
    (let ((buffer
           (or (and reuse-buffer (current-buffer))
               (generate-new-buffer (format "*dape-memory @ %s*" address)))))
      (with-current-buffer buffer
        (unless (eq major-mode 'dape-memory-mode)
          (dape-memory-mode)
          (when (dape--capable-p conn :supportsWriteMemoryRequest)
            (message (substitute-command-keys
                      "Write memory with `\\[save-buffer]'"))))
        (setq dape--memory-address address)
        (revert-buffer))
      (select-window
       (display-buffer buffer)))))

;;; Breakpoints

(dape--mouse-command dape-mouse-breakpoint-toggle
  "Toggle breakpoint at line."
  dape-breakpoint-toggle)

(dape--mouse-command dape-mouse-breakpoint-log
  "Add log breakpoint at line."
  dape-breakpoint-log)

(dape--mouse-command dape-mouse-breakpoint-expression
  "Add expression breakpoint at line."
  dape-breakpoint-expression)

(dape--mouse-command dape-mouse-breakpoint-hits
  "Add hits breakpoint at line."
  dape-breakpoint-hits)

(defvar dape-breakpoint-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] 'dape-mouse-breakpoint-toggle)
    (define-key map [left-margin mouse-1] 'dape-mouse-breakpoint-toggle)
    ;; TODO Would be nice if mouse-2 would open an menu for any
    ;;      breakpoint type (expression, log and hit).
    (define-key map [left-fringe mouse-2] 'dape-mouse-breakpoint-expression)
    (define-key map [left-margin mouse-2] 'dape-mouse-breakpoint-expression)
    (define-key map [left-fringe mouse-3] 'dape-mouse-breakpoint-log)
    (define-key map [left-margin mouse-3] 'dape-mouse-breakpoint-log)
    map)
  "Keymap for `dape-breakpoint-global-mode'.")

(define-minor-mode dape-breakpoint-global-mode
  "Adds fringe and margin breakpoint controls."
  :global t
  :lighter nil)

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

(defun dape--overlay-icon (overlay string bitmap face &optional in-margin)
  "Put STRING or BITMAP on OVERLAY with FACE.
If IN-MARGING put STRING in margin, otherwise put overlay over buffer
contents."
  (when-let ((buffer (overlay-buffer overlay)))
    (let ((before-string
           (cond
            ((and (window-system)
                  (not (eql (frame-parameter (selected-frame) 'left-fringe) 0)))
             (propertize " " 'display
                         `(left-fringe ,bitmap ,face)))
            (in-margin
             (with-current-buffer buffer
               (unless dape--original-margin
                 (setq-local dape--original-margin left-margin-width)
                 (setq left-margin-width 2)
                 (when-let ((window (get-buffer-window)))
                   (set-window-buffer window buffer))))
             (propertize " " 'display `((margin left-margin)
                                        ,(propertize string 'face face))))
            (t
             (move-overlay overlay
                           (overlay-start overlay)
                           (+ (overlay-start overlay)
                              (min
                               (length string)
                               (with-current-buffer (overlay-buffer overlay)
                                 (goto-char (overlay-start overlay))
                                 (- (line-end-position) (overlay-start overlay))))))
             (overlay-put overlay 'display "")
             (propertize string 'face face)))))
      (overlay-put overlay 'before-string before-string))))

(defun dape--breakpoint-freeze (overlay _after _begin _end &optional _len)
  "Make sure that dape OVERLAY region covers line."
  (apply 'move-overlay overlay (dape--overlay-region)))

(defun dape--breakpoints-reset ()
  "Reset breakpoints hits."
  (cl-loop for ov in dape--breakpoints do
           (overlay-put ov 'dape-verified-plist nil)
           (overlay-put ov 'dape-hits nil)))

(defun dape--breakpoints-stopped (hit-breakpoint-ids)
  "Increment `dape-hits' from array of HIT-BREAKPOINT-IDS."
  (cl-loop for id across hit-breakpoint-ids
           for ov = (cl-find id dape--breakpoints
                             :key (lambda (ov) (overlay-get ov 'dape-id)))
           when ov
           do (overlay-put ov 'dape-hits
                           (1+ (or (overlay-get ov 'dape-hits) 0)))))

(defun dape--breakpoints-at-point ()
  "Dape overlay breakpoints at point."
  (seq-filter (lambda (overlay)
                (eq 'dape-breakpoint (overlay-get overlay 'category)))
              (overlays-in (line-beginning-position) (line-end-position))))

(defun dape--breakpoint-buffer-kill-hook (&rest _)
  "Hook to remove breakpoint on buffer killed."
  (let ((breakpoints
         (alist-get (current-buffer)
                    (seq-group-by 'overlay-buffer
                                  dape--breakpoints))))
    (dolist (breakpoint breakpoints)
      (setq dape--breakpoints (delq breakpoint dape--breakpoints)))
    (dolist (conn (dape--live-connections))
      (when (dape--initialized-p conn)
        (dape--set-breakpoints-in-buffer conn (current-buffer)))))
  (run-hooks 'dape-update-ui-hook))

(cl-defun dape--breakpoint-place (&key log expression hits)
  "Place breakpoint at current line.
If LOG is non nil place log breakpoint with LOG string.
If EXPRESSION is non nil place conditional breakpoint with EXPRESSION
string.
If HITS is non nil place conditional breakpoint with HITS string.

LOG, EXPRESSION and HITS are mutually exclusive.

If there are breakpoints at current line remove those breakpoints from
`dape--breakpoints'.
Updates all breakpoints in all known connections."
  (unless (derived-mode-p 'prog-mode)
    (user-error "Trying to set breakpoint in none `prog-mode' buffer"))
  (when-let ((prev-breakpoints (dape--breakpoints-at-point)))
    (dolist (prev-breakpoint prev-breakpoints)
      (dape--breakpoint-remove prev-breakpoint 'skip-update)))
  (let ((breakpoint (apply 'make-overlay (dape--overlay-region))))
    (overlay-put breakpoint 'window t)
    (overlay-put breakpoint 'category 'dape-breakpoint)
    (cond
     (log
      (overlay-put breakpoint :log log)
      (overlay-put breakpoint 'after-string
                   (concat
                    " "
                    (propertize (format "Log: %s" log)
                                'face 'dape-log-face
                                'mouse-face 'highlight
                                'help-echo "mouse-1: edit log message"
                                'keymap
                                (let ((map (make-sparse-keymap)))
                                  (define-key map [mouse-1]
                                              #'dape-mouse-breakpoint-log)
                                  map)))))
     (expression
      (overlay-put breakpoint :expression expression)
      (overlay-put breakpoint 'after-string
                   (concat
                    " "
                    (propertize
                     (format "Cond: %s" expression)
                     'face 'dape-expression-face
                     'mouse-face 'highlight
                     'help-echo "mouse-1: edit break expression"
                     'keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mouse-1] #'dape-mouse-breakpoint-expression)
                       map)))))
     (hits
      (overlay-put breakpoint :hits hits)
      (overlay-put breakpoint 'after-string
                   (concat
                    " "
                    (propertize
                     (format "Hit %s" hits)
                     'face 'dape-hits-face
                     'mouse-face 'highlight
                     'help-echo "mouse-1: edit break hits"
                     'keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mouse-1] #'dape-mouse-breakpoint-hits)
                       map)))))
     (t
      (overlay-put breakpoint :breakpoint t)
      (dape--overlay-icon breakpoint dape-breakpoint-margin-string
                          'breakpoint 'dape-breakpoint-face 'in-margin)))
    (overlay-put breakpoint 'modification-hooks '(dape--breakpoint-freeze))
    (push breakpoint dape--breakpoints)
    (dolist (conn (dape--live-connections))
      (dape--set-breakpoints-in-buffer conn (current-buffer)))
    (add-hook 'kill-buffer-hook 'dape--breakpoint-buffer-kill-hook nil t)
    (run-hooks 'dape-update-ui-hook)
    breakpoint))

(defun dape--breakpoint-remove (overlay &optional skip-update)
  "Remove OVERLAY breakpoint from buffer and session.
When SKIP-UPDATE is non nil, does not notify adapter about removal."
  (setq dape--breakpoints (delq overlay dape--breakpoints))
  (let ((buffer (overlay-buffer overlay)))
    (delete-overlay overlay)
    (unless skip-update
      (dolist (conn (dape--live-connections))
        (dape--set-breakpoints-in-buffer conn buffer)))
    (dape--margin-cleanup buffer))
  (run-hooks 'dape-update-ui-hook))

(defun dape--breakpoint-update (conn overlay breakpoint)
  "Update breakpoint OVERLAY with BREAKPOINT plist from CONN."
  (let ((id (plist-get breakpoint :id))
        (verified (eq (plist-get breakpoint :verified) t)))
    (overlay-put overlay 'dape-id id)
    (overlay-put overlay 'dape-verified-plist
                 (plist-put (overlay-get overlay 'dape-verified-plist)
                            conn verified))
    (run-hooks 'dape-update-ui-hook))
  (when-let* ((old-buffer (overlay-buffer overlay))
              (old-line (with-current-buffer old-buffer
                          (line-number-at-pos (overlay-start overlay))))
              (breakpoint
               (append breakpoint
                       ;; Default to current overlay as `:source'
                       `(:source
                         ,(or (when-let ((path (buffer-file-name old-buffer)))
                                `(:path ,(dape--path conn path 'remote)))
                              (with-current-buffer old-buffer
                                dape--source))))))
    (dape--with-request (dape--source-ensure conn breakpoint)
      (when-let* ((marker (dape--object-to-marker conn breakpoint))
                  (new-buffer (marker-buffer marker))
                  (new-line (plist-get breakpoint :line)))
        (unless (and (= old-line new-line)
                     (eq old-buffer new-buffer))
          (with-current-buffer new-buffer
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- new-line))
              (dape-breakpoint-remove-at-point)
              (pcase-let ((`(,beg ,end) (dape--overlay-region)))
                (move-overlay overlay beg end new-buffer))
              (pulse-momentary-highlight-region (line-beginning-position)
                                                (line-beginning-position 2)
                                                'next-error)))
          (dape--repl-message
           (format "* Breakpoint in %s moved from line %s to %s *"
                   old-buffer old-line new-line))
          (run-hooks 'dape-update-ui-hook))))))

(defconst dape--breakpoint-args '(:log :expression :hits)
  "Plist keys for breakpoint serialization.")

(defun dape-breakpoint-load (&optional file)
  "Load breakpoints from FILE.
All breakpoints will be removed before loading new ones.
Will open buffers containing breakpoints.
Will use `dape-default-breakpoints-file' if FILE is nil."
  (interactive
   (list
    (read-file-name "Load breakpoints from file: ")))
  (setq file
        (or file dape-default-breakpoints-file))
  (when (file-exists-p file)
    (dape-breakpoint-remove-all)
    (cl-loop
     with breakpoints = (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (nreverse (read (current-buffer))))
     for (file point . args) in breakpoints
     for plist = (cl-mapcan 'list dape--breakpoint-args args)
     do (ignore-errors
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char point)
              (apply #'dape--breakpoint-place plist)))))))

(defun dape-breakpoint-save (&optional file)
  "Save breakpoints to FILE.
Will use `dape-default-breakpoints-file' if FILE is nil."
  (interactive
   (list
    (read-file-name "Save breakpoints to file: ")))
  (setq file
        (or file dape-default-breakpoints-file))
  (with-temp-buffer
    (insert
     ";; Generated by `dape-breakpoint-save'\n"
     ";; Load breakpoints with `dape-breakpoint-load'\n\n")
    (cl-loop
     for ov in dape--breakpoints
     for file = (buffer-file-name (overlay-buffer ov))
     for point = (overlay-start ov)
     for args = (mapcar (apply-partially 'overlay-get ov) dape--breakpoint-args)
     when (and file point)
     collect (append (list file point) args) into breakpoints
     finally do (prin1 breakpoints (current-buffer)))
    (write-file file)))


;;; Source buffers

(defun dape--source (conn plist)
  "Return source from PLIST for adapter CONN.
Source is a number, string, buffer or nil if not available."
  (let* ((source (plist-get plist :source))
         (path (plist-get source :path))
         (ref (plist-get source :sourceReference))
         (buffer (plist-get dape--source-buffers ref)))
    (or (and path (file-exists-p (dape--path conn path 'local)) path)
        (and buffer (buffer-live-p buffer) buffer)
        (and (numberp ref) (< 0 ref) ref))))

(defun dape--source-ensure (conn plist cb)
  "Ensure that source object in PLIST exist for adapter CONN.
See `dape-request' for expected CB signature."
  (pcase (dape--source conn plist)
    ((or (pred stringp) (pred bufferp)) (dape--request-return cb))
    ((and (pred numberp) source-reference)
     (let ((source (plist-get plist :source)))
       (dape--with-request-bind
           ((&key content mimeType &allow-other-keys) error)
           (dape-request conn "source"
                         (list :source source :sourceReference source-reference))
         (cond
          (error (dape--repl-message (format "%s" error) 'dape-repl-error-face))
          (content
           (let ((buffer
                  (generate-new-buffer (format "*dape-source %s*"
                                               (plist-get source :name)))))
             (setq dape--source-buffers
                   (plist-put dape--source-buffers
                              (plist-get source :sourceReference) buffer))
             (with-current-buffer buffer
               (when mimeType
                 (if-let ((mode
                           (alist-get mimeType dape-mime-mode-alist nil nil 'equal)))
                     (unless (eq major-mode mode)
                       (funcall mode))
                   (message "Unknown mime type %s, see `dape-mime-mode-alist'"
                            mimeType)))
               (setq-local buffer-read-only t
                           dape--source source)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert content))
               (goto-char (point-min)))
             (dape--request-return cb)))))))))


;;; Stack frame source

(defvar dape--overlay-arrow-position (make-marker)
  "Dape stack position marker.")

(add-to-list 'overlay-arrow-variable-list 'dape--overlay-arrow-position)

(defvar dape--stack-position-overlay nil
  "Dape stack position overlay for line.")

(defun dape--stack-frame-cleanup ()
  "Cleanup after `dape--stack-frame-display'."
  (when-let ((buffer (marker-buffer dape--overlay-arrow-position)))
    (with-current-buffer buffer
      ;; FIXME Should restore `fringe-indicator-alist'
      (dape--remove-eldoc-hook)))
  (when (overlayp dape--stack-position-overlay)
    (delete-overlay dape--stack-position-overlay))
  (set-marker dape--overlay-arrow-position nil))

(defun dape--stack-frame-display-1 (conn frame deepest-p)
  "Display FRAME for adapter CONN as if DEEPEST-p.
Helper for `dape--stack-frame-display'."
  (dape--with-request (dape--source-ensure conn frame)
    ;; An update event could have fired between call to
    ;; `dape--stack-frame-cleanup' and callback, we have make sure
    ;; that overlay is deleted before we are dropping the overlay
    ;; reference
    (dape--stack-frame-cleanup)
    (when-let ((marker (dape--object-to-marker conn frame)))
      (with-current-buffer (marker-buffer marker)
        (dape--add-eldoc-hook)
        (save-excursion
          (goto-char (marker-position marker))
          (setq dape--stack-position-overlay
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-beginning-position 2))))
                  (overlay-put ov 'face 'dape-source-line-face)
                  (when deepest-p
                    (when-let ((exception-description
                                (dape--exception-description conn)))
                      (overlay-put ov 'after-string
                                   (propertize (concat exception-description "\n")
                                               'face
                                               'dape-exception-description-face))))
                  ov)
                fringe-indicator-alist
                (unless deepest-p
                  '((overlay-arrow . hollow-right-triangle))))
          ;; Finally lets move arrow to point
          (move-marker dape--overlay-arrow-position
                       (line-beginning-position)))
        (when-let ((window
                    (display-buffer (marker-buffer marker)
                                    dape-display-source-buffer-action)))
          ;; Change selected window if not `dape-repl' buffer is selected
          (unless (with-current-buffer (window-buffer)
                    (memq major-mode '(dape-repl-mode)))
            (select-window window))
          (with-selected-window window
            ;; XXX: This code is running within timer context, which
            ;;      does not play nice with `post-command-hook'.
            ;;      Since hooks are runn'ed before the point is
            ;;      actually moved.
            (goto-char (marker-position marker))
            ;; Here we are manually intervening to account for this.
            ;; The following logic borrows from gud.el to interact
            ;; with `hl-line'.
            (when (featurep 'hl-line)
	      (cond
               (global-hl-line-mode (global-hl-line-highlight))
	       ((and hl-line-mode hl-line-sticky-flag) (hl-line-highlight))))
            (run-hooks 'dape-display-source-hook)))))))

(defun dape--stack-frame-display (conn)
  "Update stack frame arrow marker for adapter CONN.
Buffer is displayed with `dape-display-source-buffer-action'."
  (dape--stack-frame-cleanup)
  (when (dape--stopped-threads conn)
    (let* ((selected (dape--current-stack-frame conn))
           (thread (dape--current-thread conn))
           (deepest-p (eq selected (car (plist-get thread :stackFrames)))))
      (cl-flet ((displayable-frame ()
                  (cl-loop with frames = (plist-get thread :stackFrames)
                           for cell on frames for (frame . _rest) = cell
                           when (eq frame selected) return
                           (cl-loop for frame in cell when (dape--source conn frame)
                                    return frame))))
        (if-let (;; Check if frame is available, otherwise fetch all
                 (frame (displayable-frame)))
            (dape--stack-frame-display-1 conn frame deepest-p)
          (dape--with-request (dape--stack-trace conn thread dape-stack-trace-levels)
            (when-let ((frame (displayable-frame)))
              (dape--stack-frame-display-1 conn frame deepest-p))))))))


;;; Info Buffers

(defvar-local dape--info-buffer-related nil
  "List of related buffers.")
(defvar-local dape--info-buffer-identifier nil
  "Identifying var for buffers, used only in scope buffer.
Used there as scope index.")

(defvar dape--info-buffers nil
  "List containing `dape-info' buffers, might be un-live.")

(defun dape--info-buffer-list ()
  "Return all live `dape-info-parent-mode'."
  (setq dape--info-buffers
        (seq-filter 'buffer-live-p dape--info-buffers)))

(defun dape--info-buffer-p (mode &optional identifier)
  "Is buffer of MODE with IDENTIFIER.
Uses `dape--info-buffer-identifier' as IDENTIFIER."
  (and (derived-mode-p mode)
       (or (not identifier)
           (equal dape--info-buffer-identifier identifier))))

(defun dape--info-buffer-tab (&optional reversed)
  "Select next related buffer in `dape-info' buffers.
REVERSED selects previous."
  (interactive)
  (unless dape--info-buffer-related
    (user-error "No related buffers for current buffer"))
  (pcase-let* ((order-fn (if reversed 'reverse 'identity))
               (`(,mode ,id)
                (or
                 (thread-last (append dape--info-buffer-related
                                      dape--info-buffer-related)
                              (funcall order-fn)
                              (seq-drop-while (pcase-lambda (`(,mode ,id))
                                                (not (dape--info-buffer-p mode id))))
                              (cadr))
                 (car dape--info-buffer-related))))
    (gdb-set-window-buffer
     (dape--info-get-buffer-create mode id) t)))

(defvar dape-info-parent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>")
                (lambda () (interactive) (dape--info-buffer-tab t)))
    (define-key map "\t" 'dape--info-buffer-tab)
    map)
  "Keymap for `dape-info-parent-mode'.")

(defun dape--info-buffer-change-fn (&rest _rest)
  "Hook fn for `window-buffer-change-functions' to ensure update."
  (when (derived-mode-p 'dape-info-parent-mode)
    (ignore-errors (revert-buffer))))

(defvar-local dape--info-debounce-timer nil
  "Debounce context for `dape-info-parent-mode' buffers.")

(cl-defmethod dape--info-revert :around (&rest _)
  "Wrap `dape--info-revert' methods within an debounce context.
Each buffers store its own debounce context."
  (let ((buffer (current-buffer)))
    (dape--with-debounce dape--info-debounce-timer dape-ui-debounce-time
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cl-call-next-method))))))

(define-derived-mode dape-info-parent-mode special-mode ""
  "Generic mode to derive all other Dape gud buffer modes from."
  :interactive nil
  ;; Setup header-line without `buffer-revert'
  :after-hook (progn
                (dape--info-set-related-buffers)
                (dape--info-set-header-line-format))
  (setq-local buffer-read-only t
              truncate-lines t
              cursor-in-non-selected-windows nil
              dape--info-debounce-timer (timer-create)
              ;; FIXME Is `revert-buffer-in-progress-p' is not
              ;;       respected as most of the work is done in an
              ;;       callback.
              revert-buffer-function #'dape--info-revert)
  (add-hook 'window-buffer-change-functions 'dape--info-buffer-change-fn
            nil 'local)
  (when dape-info-hide-mode-line
    (setq-local mode-line-format nil))
  (buffer-disable-undo))

(defun dape--info-header (name mode id help-echo mouse-face face)
  "Helper to create buffer header.
Creates header with string NAME, mouse map to select buffer
identified with MODE and ID (see `dape--info-buffer-identifier')
with HELP-ECHO string, MOUSE-FACE and FACE."
  (propertize name 'help-echo help-echo 'mouse-face mouse-face 'face face
              'keymap
              (gdb-make-header-line-mouse-map
	       'mouse-1
	       (lambda (event) (interactive "e")
		 (save-selected-window
		   (select-window (posn-window (event-start event)))
                   (let ((buffer (dape--info-get-buffer-create mode id)))
                     (with-current-buffer buffer (revert-buffer))
                     (gdb-set-window-buffer buffer t)))))))

(defun dape--info-set-header-line-format ()
  "Helper for dape info buffers to set header line.
Header line is constructed from buffer local
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

(defun dape--info-call-update-with (fn)
  "Helper for `dape--info-revert' functions.
Erase BUFFER content and updates `header-line-format'.
FN is expected to update insert buffer contents, update
`dape--info-buffer-related' and `header-line-format'."
  ;; Save buffer as `select-window' sets buffer
  (save-current-buffer
    (when (derived-mode-p 'dape-info-parent-mode)
      ;; Would be nice with replace-buffer-contents
      ;; But it seams to messes up string properties
      (let ((line (line-number-at-pos (point) t))
            (old-window (selected-window)))
        ;; Still don't know any better way of keeping window scroll?
        (when-let ((window (get-buffer-window)))
          (select-window window))
        (save-window-excursion
          (let ((inhibit-read-only t))
            (erase-buffer)
            (funcall fn))
          (ignore-errors
            (goto-char (point-min))
            (forward-line (1- line)))
          (dape--info-set-related-buffers)
          (dape--info-set-header-line-format))
        (when old-window
          (select-window old-window))))))

(defmacro dape--info-update-with (&rest body)
  "Create an update function from BODY.
See `dape--info-call-update-with'."
  (declare (indent 0))
  `(dape--info-call-update-with (lambda () ,@body)))

(defun dape--info-get-live-buffer (mode &optional identifier)
  "Get live dape info buffer with MODE and IDENTIFIER."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (dape--info-buffer-p mode identifier)))
            (dape--info-buffer-list)))

(defun dape--info-get-buffer-create (mode &optional identifier)
  "Get or create info buffer with MODE and IDENTIFIER."
  (let* ((identifier (or identifier 0))
         (buffer
          (or (dape--info-get-live-buffer mode identifier)
              (get-buffer-create (dape--info-buffer-name mode identifier)))))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (funcall mode)
        (setq dape--info-buffer-identifier identifier)
        (push buffer dape--info-buffers)))
    buffer))

(defun dape-info-update ()
  "Update and display `dape-info-*' buffers."
  (dolist (buffer (dape--info-buffer-list))
    (when (get-buffer-window buffer)
      (with-current-buffer buffer
        (revert-buffer)))))

(defun dape-info (&optional maybe-kill kill)
  "Update and display *dape-info* buffers.
When called interactively MAYBE-KILL is non nil.
When MAYBE-KILL is non nil kill buffers if all *dape-info* buffers
are already displayed.
When KILL is non nil kill buffers *dape-info* buffers.

See `dape-info-buffer-window-groups' to customize which buffers get
displayed."
  (interactive (list t))
  (cl-labels ((kill-dape-info ()
                (dolist (buffer (buffer-list))
                  (when (with-current-buffer buffer
                          (derived-mode-p 'dape-info-parent-mode))
                    (kill-buffer buffer)))))
    (if kill
        (kill-dape-info)
      (let (buffer-displayed-p)
        (dolist (group dape-info-buffer-window-groups)
          (unless (seq-find (lambda (buffer)
                              (and (get-buffer-window buffer)
                                   (with-current-buffer buffer
                                     (apply 'derived-mode-p group))))
                            (dape--info-buffer-list))
            (setq buffer-displayed-p t)
            (dape--display-buffer
             (dape--info-get-buffer-create (car group)))))
        (dape-info-update)
        (when (and maybe-kill (not buffer-displayed-p))
          (kill-dape-info))))))

(defconst dape--info-buffer-name-alist
  '((dape-info-breakpoints-mode . "Breakpoints")
    (dape-info-threads-mode . "Threads")
    (dape-info-stack-mode . "Stack")
    (dape-info-modules-mode . "Modules")
    (dape-info-sources-mode . "Sources")
    (dape-info-watch-mode . "Watch"))
  "Lookup for `dape-info-parent-mode' derived modes names.")

(defun dape--info-buffer-name (mode &optional identifier)
  "Create buffer name from MODE and IDENTIFIER."
  (cond
   ((eq 'dape-info-scope-mode mode)
    (concat "*dape-info Scope*"
            (unless (zerop identifier)
              (format "<%s>" identifier))))
   ((format "*dape-info %s*"
            (or (alist-get mode dape--info-buffer-name-alist)
                (error "Unable to create mode from %s with %s" mode identifier))))))

(defun dape--info-set-related-buffers ()
  "Store related buffers in `dape--info-buffer-related'."
  (setq dape--info-buffer-related
        (cl-loop with group =
                 (cl-find-if (lambda (group)
                               (apply 'derived-mode-p group))
                             dape-info-buffer-window-groups)
                 with conn = (dape--live-connection 'stopped t)
                 with scopes = (plist-get (dape--current-stack-frame conn)
                                          :scopes)
                 for mode in group
                 append
                 (cond
                  ((and (not scopes) (eq mode 'dape-info-scope-mode))
                   ;; TODO Should grab `dape--info-buffer-related'
                   ;;      from other buffers if there are no
                   ;;      `dape-info-scope-modes' in the current ctx.
                   ;;      This would fix tabbing into other scopes after
                   ;;      an adapter has been killed.
                   (seq-filter (lambda (related)
                                 (eq (car related) 'dape-info-scope-mode))
                               dape--info-buffer-related))
                  ((eq mode 'dape-info-scope-mode)
                   (cl-loop for scope in scopes
                            for i from 0
                            for name = (plist-get scope :name)
                            collect
                            (list 'dape-info-scope-mode i name)))
                  (t
                   `((,mode nil
                            ,(alist-get mode dape--info-buffer-name-alist))))))))


;;; Info breakpoints buffer

(dape--command-at-line dape-info-breakpoint-goto (dape--info-breakpoint)
  "Goto breakpoint at line in dape info buffer."
  (when-let* ((buffer (overlay-buffer dape--info-breakpoint)))
    (with-selected-window (display-buffer buffer dape-display-source-buffer-action)
      (goto-char (overlay-start dape--info-breakpoint)))))

(dape--command-at-line dape-info-breakpoint-delete (dape--info-breakpoint)
  "Delete breakpoint at line in dape info buffer."
  (dape--breakpoint-remove dape--info-breakpoint)
  (dape--display-buffer (dape--info-get-buffer-create 'dape-info-breakpoints-mode)))

(dape--command-at-line dape-info-breakpoint-log-edit (dape--info-breakpoint)
  "Edit breakpoint at line in dape info buffer."
  (let ((edit-fn
         (cond
          ((overlay-get dape--info-breakpoint :log)
           'dape-breakpoint-log)
          ((overlay-get dape--info-breakpoint :expression)
           'dape-breakpoint-expression)
          ((overlay-get dape--info-breakpoint :hits)
           'dape-breakpoint-hits)
          ((user-error "Unable to edit breakpoint on line without log or expression breakpoint")))))
    (when-let* ((buffer (overlay-buffer dape--info-breakpoint)))
      (with-selected-window (display-buffer buffer dape-display-source-buffer-action)
        (goto-char (overlay-start dape--info-breakpoint))
        (call-interactively edit-fn)))))

(dape--buffer-map dape-info-breakpoints-line-map dape-info-breakpoint-goto
  (define-key map "D" 'dape-info-breakpoint-delete)
  (define-key map "d" 'dape-info-breakpoint-delete)
  (define-key map "e" 'dape-info-breakpoint-log-edit))

(dape--command-at-line dape-info-data-breakpoint-delete (dape--info-data-breakpoint)
  "Delete data breakpoint at line in info buffer."
  (setq dape--data-breakpoints
        (delq dape--info-data-breakpoint
              dape--data-breakpoints))
  (when-let ((conn (dape--live-connection 'stopped t)))
    (dape--with-request (dape--set-data-breakpoints conn)))
  (run-hooks 'dape-update-ui-hook))

(dape--buffer-map dape-info-data-breakpoints-line-map nil
  (define-key map "D" 'dape-info-data-breakpoint-delete)
  (define-key map "d" 'dape-info-data-breakpoint-delete))

(dape--command-at-line dape-info-exceptions-toggle (dape--info-exception)
  "Toggle exception at line in dape info buffer."
  (plist-put dape--info-exception :enabled
             (not (plist-get dape--info-exception :enabled)))
  (dape-info-update)
  (dolist (conn (dape--live-connections))
    (dape--set-exception-breakpoints conn)))

(dape--buffer-map dape-info-exceptions-line-map dape-info-exceptions-toggle)

(define-derived-mode dape-info-breakpoints-mode dape-info-parent-mode
  "Breakpoints"
  :interactive nil
  "Major mode for Dape info breakpoints.")

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-breakpoints-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-breakpoints-mode'."
  (dape--info-update-with
    (let ((table (make-gdb-table))
          ;; Show Hits only when it's supported e.i. when breakpoint
          ;; id is included in the "stopped" event.
          (with-hits-p
           (cl-find-if (lambda (ov)
                         (when-let ((hits (overlay-get ov 'dape-hits)))
                           (> hits 0)))
                       dape--breakpoints)))
      (gdb-table-add-row table
                         (list "A" "Type" "Where/On" (when with-hits-p "Hit")))
      (cl-loop
       for breakpoint in dape--breakpoints
       for buffer = (overlay-buffer breakpoint)
       for verified-plist = (overlay-get breakpoint 'dape-verified-plist)
       for verified-p = (or
                         ;; No live connection show every breakpoint
                         ;; as verified
                         (not (dape--live-connection 'last t))
                         (cl-find-if (apply-partially 'plist-get verified-plist)
                                     (dape--live-connections)))
       for line = (with-current-buffer buffer
                    (line-number-at-pos (overlay-start breakpoint)))
       do (gdb-table-add-row
           table
           (list
            (if verified-p
                (propertize "y" 'font-lock-face font-lock-warning-face)
              "n")
            (cond
             ((overlay-get breakpoint :log) "Log")
             ((overlay-get breakpoint :hits) "Hits")
             ((overlay-get breakpoint :expression) "Cond")
             (t "Break"))
            (concat
             (if-let (file (buffer-file-name buffer))
                 (dape--format-file-line file line)
               (buffer-name buffer))
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (overlay-start breakpoint))
                 (truncate-string-to-width
                  (concat " " (string-trim (or (thing-at-point 'line) "")))
                  dape-info-breakpoint-source-line-max))))
            (when-let* (with-hits-p
                        (hits (overlay-get breakpoint 'dape-hits)))
              (format "%s" hits)))
           (append
            (list
             'dape--info-breakpoint breakpoint
             'keymap dape-info-breakpoints-line-map
             'mouse-face 'highlight
             'help-echo "mouse-2, RET: visit breakpoint")
            (unless verified-p
              '(face shadow)))))
      (cl-loop
       for plist in dape--data-breakpoints do
       (gdb-table-add-row
        table
        (list
         (propertize "y" 'font-lock-face font-lock-warning-face)
         "Data"
         (format "%s %s %s"
                 (propertize
                  (plist-get plist :name)
                  'font-lock-face
                  'font-lock-variable-name-face)
                 (plist-get plist :accessType)
                 (when-let ((data-id (plist-get plist :dataId)))
                   (format "(%s)" data-id)))
         nil)
        (list 'dape--info-data-breakpoint plist
              'keymap dape-info-data-breakpoints-line-map)))
      (cl-loop
       for exception in dape--exceptions do
       (gdb-table-add-row
        table
        (list
         (if (plist-get exception :enabled)
             (propertize "y" 'font-lock-face font-lock-warning-face)
           (propertize "n" 'font-lock-face font-lock-doc-face))
         "Excep"
         (format "%s" (plist-get exception :label))
         nil)
        (list
         'dape--info-exception exception
         'mouse-face 'highlight
         'keymap dape-info-exceptions-line-map
         'help-echo "mouse-2, RET: toggle exception")))
      (setf (gdb-table-right-align table) t)
      (insert (gdb-table-string table " ")))))


;;; Info threads buffer

(defvar dape--info-thread-position nil
  "`dape-info-thread-mode' marker for `overlay-arrow-variable-list'.")
(defvar dape-info--threads-bench nil
  "List of benched connections.")
(defvar dape-info--threads-tt-bench 2
  "Time to Bench.")

(dape--command-at-line dape-info-select-thread (dape--info-thread dape--info-conn)
  "Select thread at line in dape info buffer."
  (dape-select-thread dape--info-conn (plist-get dape--info-thread :id)))

(defvar dape--info-threads-font-lock-keywords
  (append gdb-threads-font-lock-keywords
          '((" \\(unknown\\)"  (1 font-lock-warning-face))
            (" \\(exited\\)"  (1 font-lock-warning-face))
            (" \\(started\\)"  (1 font-lock-string-face))))
  "Keywords for `dape-info-threads-mode'.")

(dape--buffer-map dape-info-threads-line-map dape-info-select-thread
  ;; TODO Add bindings for individual threads.
  )

(defun dape--info-threads-stack-info (conn cb)
  "Populate stack frame info for CONNs threads.
See `dape-request' for expected CB signature."
  (let (threads)
    (cond
     ;; Current CONN is benched
     ((member conn dape-info--threads-bench)
      (dape--request-return cb))
     ;; Stopped threads
     ((setq threads
            (cl-remove-if (lambda (thread)
                            (plist-get thread :request-in-flight))
                          (dape--stopped-threads conn)))
      (let ((start-time (current-time))
            (responses 0))
        (dolist (thread threads)
          ;; HACK Keep track of requests in flight as `revert-buffer'
          ;;      might be called at any time, and we want keep
          ;;      unnecessary chatter at a minimum.
          ;; NOTE This is hack is still necessary if user sets
          ;;      `dape-ui-debounce-time' to 0.0.
          (plist-put thread :request-in-flight t)
          (dape--with-request (dape--stack-trace conn thread 1)
            (plist-put thread :request-in-flight nil)
            ;; Time response, if slow bench that CONN
            (when (and (time-less-p (timer-relative-time
                                     start-time dape-info--threads-tt-bench)
                                    (current-time))
                       (not (member conn dape-info--threads-bench)))
              (dape--repl-message
               "* Disabling stack trace info in Threads buffer for connection (slow) *"
               'dape-repl-error-face)
              (push conn dape-info--threads-bench))
            ;; When all request have resolved return
            (when (length= threads (setf responses (1+ responses)))
              (dape--request-return cb))))))
     ;; No stopped threads
     (t (dape--request-return cb))))
  ;; House keeping, no need to keep dead connections in bench
  (when dape-info--threads-bench
    (let ((conns (dape--live-connections)))
      (cl-delete-if-not (lambda (conn) (member conn conns))
                        dape-info--threads-bench))))

(define-derived-mode dape-info-threads-mode dape-info-parent-mode "Threads"
  "Major mode for dape info threads."
  :interactive nil
  (setq font-lock-defaults '(dape--info-threads-font-lock-keywords)
        dape--info-thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'dape--info-thread-position))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-threads-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-threads-mode'."
  (if-let ((conn (dape--live-connection 'last t)))
      (dape--with-request (dape--info-threads-stack-info conn)
        (cl-loop
         initially do (set-marker dape--info-thread-position nil)
         with table = (make-gdb-table)
         with conns = (dape--live-connections)
         with current-thread = (dape--current-thread conn)
         with conn-prefix-p = (length> (cl-remove-if-not 'dape--threads conns) 1)
         with line-count = 0
         with selected-line = nil
         for conn in conns
         for index upfrom 1 do
         (cl-loop
          for thread in (dape--threads conn) do
          (setq line-count (1+ line-count))
          (when (eq current-thread thread)
            (setq selected-line line-count))
          (gdb-table-add-row
           table
           (append
            (when conn-prefix-p
              (list (format "%s:" index)))
            (list (format "%s" (plist-get thread :id)))
            (list
             (concat
              (when dape-info-thread-buffer-verbose-names
                (concat (plist-get thread :name) " "))
              (or (and-let* ((status (plist-get thread :status)))
                    (format "%s" status))
                  "unknown")
              ;; Include frame information for stopped threads
              (if-let* (((equal (plist-get thread :status) 'stopped))
                        (top-stack (car (plist-get thread :stackFrames))))
                  (concat
                   " in " (plist-get top-stack :name)
                   (when-let* ((dape-info-thread-buffer-locations)
                               (path (thread-first top-stack
                                                   (plist-get :source)
                                                   (plist-get :path)))
                               (path (dape--path conn path 'local))
                               (line (plist-get top-stack :line)))
                     (concat " of " (dape--format-file-line path line)))
                   (when-let ((dape-info-thread-buffer-addresses)
                              (addr
                               (plist-get top-stack :instructionPointerReference)))
                     (concat " at " addr))
                   " ")))))
           (list
            'dape--info-conn conn
            'dape--info-thread thread
            'mouse-face 'highlight
            'keymap dape-info-threads-line-map
            'help-echo "mouse-2, RET: select thread")))
         finally do
         (dape--info-update-with
           (insert (gdb-table-string table " "))
           (when selected-line
             (gdb-mark-line selected-line dape--info-thread-position)))))
    (dape--info-update-with
      (set-marker dape--info-thread-position nil)
      (insert "No thread information available."))))


;;; Info stack buffer

(defvar dape--info-stack-position nil
  "`dape-info-stack-mode' marker for `overlay-arrow-variable-list'.")

(defvar dape--info-stack-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--command-at-line dape-info-stack-select (dape--info-frame)
  "Select stack at line in dape info buffer."
  (dape-select-stack (dape--live-connection 'stopped)
                     (plist-get dape--info-frame :id)))

(dape--buffer-map dape-info-stack-line-map dape-info-stack-select)

(define-derived-mode dape-info-stack-mode dape-info-parent-mode "Stack"
  "Major mode for Dape info stack."
  :interactive nil
  (setq font-lock-defaults '(dape--info-stack-font-lock-keywords)
        dape--info-stack-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'dape--info-stack-position))

(defun dape--info-stack-buffer-insert (conn current-stack-frame stack-frames)
  "Helper for inserting stack info into *dape-info Stack* buffer.
Create table from CURRENT-STACK-FRAME and STACK-FRAMES and insert into
current buffer with CONN config."
  (cl-loop with table = (make-gdb-table)
           for frame in stack-frames do
           (gdb-table-add-row
            table
            (list
             "in"
             (concat
              (plist-get frame :name)
              (when-let* ((dape-info-stack-buffer-locations)
                          (path (thread-first frame
                                              (plist-get :source)
                                              (plist-get :path)))
                          (path (dape--path conn path 'local)))
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
           finally (gdb-mark-line line dape--info-stack-position)))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-stack-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-stack-mode'."
  (let* ((conn (or (dape--live-connection 'stopped t t)
                   (dape--live-connection 'last t t)))
         (current-thread (dape--current-thread conn))
         (stack-frames (plist-get current-thread :stackFrames))
         (current-stack-frame (dape--current-stack-frame conn)))
    (cond
     ((or (not current-stack-frame)
          (not (dape--stopped-threads conn)))
      (dape--info-update-with
        (set-marker dape--info-stack-position nil)
        (cond
         (current-thread
          (insert (format "Thread \"%s\" is not stopped."
                          (plist-get current-thread :name))))
         (t
          (insert "No stack information.")))))
     (t
      ;; Why are we updating it twice? Calls to `dape--stack-trace'
      ;; with an large nof can be expensive, therefore 1 nof is fetchd
      ;; at an 'update event, then we fetch the rest here.

      ;; Start off with shoving available stack info into buffer
      (dape--info-update-with
        (dape--info-stack-buffer-insert conn current-stack-frame stack-frames))
      (dape--with-request
          (dape--stack-trace conn current-thread dape-stack-trace-levels)
        ;; If stack trace lookup with `dape-stack-trace-levels' frames changed
        ;; the stack frame list, we need to update the buffer again
        (unless (eq stack-frames (plist-get current-thread :stackFrames))
          (dape--info-update-with
            (dape--info-stack-buffer-insert conn current-stack-frame
                                            (plist-get current-thread :stackFrames)))))))))


;;; Info modules buffer

(defvar dape--info-modules-font-lock-keywords
  '(("^\\([^ ]+\\) "  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--command-at-line dape-info-modules-goto (dape--info-module)
  "Goto module."
  (let ((conn (dape--live-connection 'last t))
        (source (list :source dape--info-module)))
    (dape--with-request (dape--source-ensure conn source)
      (if-let ((marker
                (dape--object-to-marker conn source)))
          (pop-to-buffer (marker-buffer marker))
        (user-error "Unable to open module")))))

(dape--buffer-map dape-info-module-line-map dape-info-modules-goto)

(define-derived-mode dape-info-modules-mode dape-info-parent-mode "Modules"
  "Major mode for Dape info modules."
  :interactive nil
  (setq font-lock-defaults '(dape--info-modules-font-lock-keywords)))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-modules-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-modules-mode'."
  (dape--info-update-with
    ;; Use last connection if current is dead
    (when-let ((conn (or (dape--live-connection 'stopped t)
                         (dape--live-connection 'last t)
                         dape--connection)))
      (cl-loop with modules = (dape--modules conn)
               with table = (make-gdb-table)
               for module in (reverse modules)
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
               finally (insert (gdb-table-string table " "))))))


;;; Info sources buffer

(dape--command-at-line dape-info-sources-goto (dape--info-source)
  "Goto source."
  (let ((conn (dape--live-connection 'last t))
        (source (list :source dape--info-source)))
    (dape--with-request (dape--source-ensure conn source)
      (if-let ((marker
                (dape--object-to-marker conn source)))
          (pop-to-buffer (marker-buffer marker))
        (user-error "Unable to get source")))))

(dape--buffer-map dape-info-sources-line-map dape-info-sources-goto)

(define-derived-mode dape-info-sources-mode dape-info-parent-mode "Sources"
  "Major mode for Dape info sources."
  :interactive nil)

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-sources-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-sources-mode'."
  (dape--info-update-with
    ;; Use last connection if current is dead
    (when-let ((conn (or (dape--live-connection 'stopped t)
                         (dape--live-connection 'last t)
                         dape--connection)))
      (cl-loop with sources = (dape--sources conn)
               with table = (make-gdb-table)
               for source in (reverse sources) do
               (gdb-table-add-row
                table (list (concat (plist-get source :name) " "))
                (list 'dape--info-source source
                      'mouse-face 'highlight
                      'keymap dape-info-sources-line-map
                      'help-echo "mouse-2, RET: goto source"))
               finally (insert (gdb-table-string table " "))))))


;;; Info scope buffer

(defvar dape--info-expanded-p (make-hash-table :test 'equal)
  "Hash table to keep track of expanded info variables.")

(defun dape--info-expanded-p (path object)
  (and (not (eq (plist-get object :expensive) t))
       (gethash (cons (plist-get object :name) path)
                dape--info-expanded-p)))

(dape--command-at-line dape-info-scope-toggle (dape--info-path)
  "Expand or contract variable at line in dape info buffer."
  (unless (dape--live-connection 'stopped)
    (user-error "No stopped threads"))
  (puthash dape--info-path (not (gethash dape--info-path dape--info-expanded-p))
           dape--info-expanded-p)
  (revert-buffer))

(dape--buffer-map dape-info-variable-prefix-map dape-info-scope-toggle)

(dape--command-at-line dape-info-scope-watch-dwim (dape--info-variable)
  "Watch variable or remove from watch at line in dape info buffer."
  (dape-watch-dwim (or (plist-get dape--info-variable :evaluateName)
                       (plist-get dape--info-variable :name))
                   (eq major-mode 'dape-info-watch-mode)
                   (eq major-mode 'dape-info-scope-mode))
  (when (derived-mode-p 'dape-info-parent-mode)
    (gdb-set-window-buffer
     (dape--info-get-buffer-create 'dape-info-watch-mode) t)))

(dape--buffer-map dape-info-variable-name-map dape-info-scope-watch-dwim)

(dape--command-at-line dape-info-variable-edit
  (dape--info-ref dape--info-variable)
  "Edit variable value at line in dape info buffer."
  (dape--set-variable (dape--live-connection 'stopped)
                      dape--info-ref
                      dape--info-variable
                      (read-string
                       (format "Set value of %s `%s' = "
                               (plist-get dape--info-variable :type)
                               (plist-get dape--info-variable :name))
                       (or (plist-get dape--info-variable :value)
                           (plist-get dape--info-variable :result)))))

(dape--buffer-map dape-info-variable-value-map dape-info-variable-edit)

(dape--command-at-line dape-info-scope-data-breakpoint (dape--info-ref dape--info-variable)
  "Add data breakpoint on variable at line in info buffer."
  (let ((conn (dape--live-connection 'stopped))
        (name (or (plist-get dape--info-variable :evaluateName)
                  (plist-get dape--info-variable :name))))
    (unless (dape--capable-p conn :supportsDataBreakpoints)
      (user-error "Adapter does not support data breakpoints"))
    (dape--with-request-bind
        ;; TODO Test if canPersist works, have not found an adapter
        ;;      supporting it.
        ((&key dataId description accessTypes &allow-other-keys) error)
        (dape-request conn "dataBreakpointInfo"
                      (if (eq dape--info-ref 'watch)
                          (list :name name
                                :frameId (plist-get (dape--current-stack-frame conn) :id))
                        (list :variablesReference dape--info-ref
                              :name name)))
      (if (or error (not (stringp dataId)))
          (message "Unable to set data breakpoint: %s" (or error description))
        (push (list :name name
                    :dataId dataId
                    :accessType (completing-read
                                 (format "Breakpoint type for `%s': " name)
                                 (append accessTypes nil) nil t))
              dape--data-breakpoints)
        (dape--with-request
            (dape--set-data-breakpoints conn)
          ;; Make sure breakpoint buffer is displayed
          (dape--display-buffer
           (dape--info-get-buffer-create 'dape-info-breakpoints-mode))
          (run-hooks 'dape-update-ui-hook))))))

(defvar dape-info-scope-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'dape-info-scope-toggle)
    (define-key map "W" 'dape-info-scope-watch-dwim)
    (define-key map "=" 'dape-info-variable-edit)
    (define-key map "b" 'dape-info-scope-data-breakpoint)
    map)
  "Local keymap for dape scope buffers.")

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
                  (substring prop 0 (string-match-p "\n" prop))))
          (if (and (> max 0) (length> prop max))
              (push (propertize (string-truncate-left prop max) 'help-echo prop-org)
                    columns)
            (push prop columns)))))
    (nreverse columns)))

(defun dape--info-scope-add-variable (table object ref path expanded-p maps)
  "Add variable OBJECT with REF and PATH to TABLE.
EXPANDED-P is called with PATH and OBJECT to determine if function
should continue to be called recursively.
MAPS is an PLIST where the VALUES add `keymaps' to `name', `value'
or `prefix' part of variable string."
  (let* ((name (or (plist-get object :name) ""))
         (type (or (plist-get object :type) ""))
         (value (or (plist-get object :value)
                    (plist-get object :result)
                    " "))
         (prefix (make-string (* (1- (length path)) 2) ?\s))
         (expanded (funcall expanded-p path object))
         (path (cons (plist-get object :name) path))
         row)
    (setq name
          (apply 'propertize name
                 'font-lock-face font-lock-variable-name-face
                 'face font-lock-variable-name-face
                 (when-let ((map (plist-get maps 'name)))
                   (list 'mouse-face 'highlight
                         'help-echo "mouse-2: create or remove watch expression"
                         'keymap map)))
          type
          (propertize type
                      'font-lock-face font-lock-type-face
                      'face font-lock-type-face)
          value
          (apply 'propertize value
                 (when-let ((map (plist-get maps 'value)))
                   (list 'mouse-face 'highlight
                         'help-echo "mouse-2: edit value"
                         'keymap map)))
          prefix
          (let ((map (plist-get maps 'prefix)))
            (cond
             ((not map) prefix)
             ((zerop (or (plist-get object :variablesReference) 0))
              (concat prefix "  "))
             ((and expanded (plist-get object :variables))
              (concat
               (propertize (concat prefix "-")
                           'mouse-face 'highlight
                           'help-echo "mouse-2: contract"
                           'keymap map)
               " "))
             (t
              (concat
               (propertize (concat prefix "+")
                           'mouse-face 'highlight
                           'help-echo "mouse-2: expand"
                           'keymap map)
               " ")))))
    (setq row (dape--info-locals-table-columns-list
               `((name  . ,name)
                 (type  . ,type)
                 (value . ,value))))
    (setcar row (concat prefix (car row)))
    (gdb-table-add-row table
                       (if dape-info-variable-table-aligned
                           row
                         (list (mapconcat 'identity row " ")))
                       (list 'dape--info-variable object
                             'dape--info-path path
                             'dape--info-ref ref))
    (when expanded
      ;; TODO Should be paged
      (dolist (variable (plist-get object :variables))
        (dape--info-scope-add-variable
         table variable (plist-get object :variablesReference)
         path expanded-p maps)))))

;; FIXME Empty header line when adapter is killed
(define-derived-mode dape-info-scope-mode dape-info-parent-mode "Scope"
  "Major mode for Dape info scope."
  :interactive nil)

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-scope-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-scope-mode'."
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t)))
              (frame (dape--current-stack-frame conn))
              (scopes (plist-get frame :scopes))
              ;; FIXME if scope is out of range here scope list could
              ;;       have shrunk since last update and current
              ;;       scope buffer should be killed and replaced if
              ;;       if visible
              (scope (nth dape--info-buffer-identifier scopes))
              ;; Check for stopped threads to reduce flickering
              ((dape--stopped-threads conn)))
    (dape--with-request (dape--variables conn scope)
      (dape--with-request
          (dape--variables-recursive conn scope (list (plist-get scope :name))
                                     #'dape--info-expanded-p)
        (when (and scope scopes (dape--stopped-threads conn))
          (dape--info-update-with
            (rename-buffer
             (format "*dape-info Scope: %s*" (plist-get scope :name)) t)
            (cl-loop
             with table = (make-gdb-table)
             for object in (plist-get scope :variables)
             initially do
             (setf (gdb-table-right-align table)
                   dape-info-variable-table-aligned)
             do
             (dape--info-scope-add-variable
              table
              object
              (plist-get scope :variablesReference)
              (list (plist-get scope :name))
              #'dape--info-expanded-p
              (list 'name dape-info-variable-name-map
                    'value dape-info-variable-value-map
                    'prefix dape-info-variable-prefix-map))
             finally (insert (gdb-table-string table " ")))))))))


;;; Info watch buffer

(defvar dape-info-watch-mode-map
  (let ((map (copy-keymap dape-info-scope-mode-map)))
    (define-key map "\C-x\C-q" 'dape-info-watch-edit-mode)
    map)
  "Local keymap for dape watch buffer.")

(define-derived-mode dape-info-watch-mode dape-info-parent-mode "Watch"
  "Major mode for Dape info watch."
  :interactive nil)

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-watch-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-watch-mode'."
  (let ((conn (dape--live-connection 'stopped t)))
    (cond
     ((not dape--watched)
      (dape--info-update-with
        (insert "No watched variable.")))
     (conn
      (let ((frame (dape--current-stack-frame conn))
            (responses 0))
        (dolist (plist dape--watched)
          (plist-put plist :variablesReference nil)
          (plist-put plist :variables nil)
          (dape--with-request-bind
              (body error)
              (dape--evaluate-expression conn
                                         (plist-get frame :id)
                                         (plist-get plist :name)
                                         "watch")
            (unless error
              (cl-loop for (key value) on body by 'cddr
                       do (plist-put plist key value)))
            (when (length= dape--watched (setf responses (1+ responses)))
              (dape--with-request
                  (dape--variables-recursive conn
                                             ;; Fake variables object
                                             (list :variables dape--watched)
                                             (list "Watch")
                                             #'dape--info-expanded-p)
                (dape--info-update-with
                  (cl-loop with table = (make-gdb-table)
                           for watch in dape--watched
                           initially (setf (gdb-table-right-align table)
                                           dape-info-variable-table-aligned)
                           do
                           (dape--info-scope-add-variable table watch 'watch (list "Watch")
                                                          #'dape--info-expanded-p
                                                          (list 'name dape-info-variable-name-map
                                                                'value dape-info-variable-value-map
                                                                'prefix dape-info-variable-prefix-map))
                           finally (insert (gdb-table-string table " "))))))))))
     (t
      (dape--info-update-with
        (cl-loop with table = (make-gdb-table)
                 for watch in dape--watched
                 initially (setf (gdb-table-right-align table)
                                 dape-info-variable-table-aligned)
                 do
                 (dape--info-scope-add-variable table watch 'watch (list "Watch")
                                                #'dape--info-expanded-p
                                                (list 'name dape-info-variable-name-map
                                                      'value dape-info-variable-value-map
                                                      'prefix dape-info-variable-prefix-map))
                 finally (insert (gdb-table-string table " "))))))))

(defvar dape--info-watch-edit-font-lock-keywords
  '(("\\(.+\\)"  (1 font-lock-variable-name-face))))

(defvar dape-info-watch-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'dape-info-watch-finish-edit)
    (define-key map "\C-c\C-k" 'dape-info-watch-abort-changes)
    map)
  "Local keymap for dape watch buffer in edit mode.")

(define-derived-mode dape-info-watch-edit-mode dape-info-watch-mode "Watch Edit"
  "Major mode for editing watch info."
  (set-buffer-modified-p nil)
  (setq revert-buffer-function #'dape--info-revert)
  (revert-buffer)
  (setq buffer-undo-list nil
        buffer-read-only nil
        font-lock-defaults '(dape--info-watch-edit-font-lock-keywords))
  (message "%s" (substitute-command-keys
	         "Press \\[dape-info-watch-finish-edit] when finished \
or \\[dape-info-watch-abort-changes] to abort changes")))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-watch-edit-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-watch-edit-mode'."
  (dape--info-update-with
    (cl-loop for watch in dape--watched
             for name = (plist-get watch :name)
             do (insert "  " name "\n"))))

(defun dape-info-watch-abort-changes ()
  "Abort change and return to `dape-info-watch-mode'."
  (interactive)
  (dape-info-watch-mode)
  (revert-buffer))

(defun dape-info-watch-finish-edit ()
  "Update watched variables and return to `dape-info-watch-mode'."
  (interactive)
  (setq dape--watched
        (cl-loop for line in (split-string (buffer-string) "[\r\n]+")
                 for trimed-line = (string-trim line)
                 unless (string-empty-p trimed-line) collect
                 (list :name trimed-line)))
  (dape-info-watch-abort-changes))


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
          (message "%s" (propertize (format "%s" (string-trim msg)) 'face face)))
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
                  (insert (apply 'propertize msg
                                 (when face (list 'font-lock-face face)))))
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

(defun dape--repl-update-variable (point variable)
  "Insert VARIABLE at POINT in *dape-repl* buffer.
VARIABLE is expected to be the string representation of a varable."
  (cond
   (dape--repl-insert-text-guard
    (run-with-timer 0.01 nil 'dape--repl-update-variable
                    point variable))
   (t
    (let ((dape--repl-insert-text-guard t))
      (when-let ((buffer (get-buffer "*dape-repl*")))
        (with-current-buffer buffer
          (when-let ((start
                      (save-excursion
                        (previous-single-property-change point
                                                         'dape--repl-variable)))
                     (end
                      (save-excursion
                        (next-single-property-change point
                                                     'dape--repl-variable))))
            (save-window-excursion
              (let ((inhibit-read-only t)
                    (line (line-number-at-pos (point) t)))
                (delete-region start end)
                (goto-char start)
                (insert variable)
                (ignore-errors
                  (goto-char (point-min))
                  (forward-line (1- line))))))))))))

(dape--command-at-line dape-repl-scope-toggle (dape--info-path
                                               dape--repl-variable)
  "Expand or contract variable at line in dape repl buffer."
  (unless (dape--live-connection 'stopped)
    (user-error "No stopped threads"))
  (puthash dape--info-path (not (gethash dape--info-path dape--info-expanded-p))
           dape--info-expanded-p)
  (dape--repl-create-variable-table (or (dape--live-connection 'stopped t)
                                        (dape--live-connection 'last))
                                    dape--repl-variable
                                    (apply-partially #'dape--repl-update-variable
                                                     (1+ (point)))))

(dape--buffer-map dape-repl-variable-prefix-map dape-repl-scope-toggle)

(defun dape--repl-create-variable-table (conn variable cb)
  "Create VARIABLE string representation with CONN.
Call CB with the variable as string for insertion into *dape-repl*."
  (dape--with-request (dape--variables conn variable)
    (dape--with-request
        (dape--variables-recursive conn variable
                                   (list (plist-get variable :name) "REPL")
                                   #'dape--info-expanded-p)
      (let ((table (make-gdb-table)))
        (setf (gdb-table-right-align table)
              dape-info-variable-table-aligned)
        (dape--info-scope-add-variable table variable
                                       'watch
                                       '("REPL")
                                       #'dape--info-expanded-p
                                       (list 'name dape-info-variable-name-map
                                             'value dape-info-variable-value-map
                                             'prefix dape-repl-variable-prefix-map))
        (funcall cb (propertize (gdb-table-string table " ")
                                'dape--repl-variable variable))))))

(defun dape--repl-shorthand-alist ()
  "Return shorthanded version of `dape-repl-commands'."
  (cl-loop for (str . command) in dape-repl-commands
           for shorthand = (cl-loop for i from 1 upto (length str)
                                    for shorthand = (substring str 0 i)
                                    unless (assoc shorthand shorthand-alist)
                                    return shorthand)
           collect (cons shorthand command) into shorthand-alist
           finally return shorthand-alist))

(defun dape--repl-input-sender (dummy-process input)
  "Dape repl `comint-input-sender'.
Send INPUT to DUMMY-PROCESS."
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
                     (cdr (assoc input (dape--repl-shorthand-alist))))))
      (dape--repl-insert-prompt)
      ;; HACK: Special handing of `dape-quit', `comint-send-input'
      ;;       expects buffer to be still live after calling
      ;;       `comint-input-sender'.  Kill buffer with timer instead
      ;;       to avoid error signal.
      (if (eq 'dape-quit cmd)
          (run-with-timer 0 nil 'call-interactively #'dape-quit)
	(call-interactively cmd)))
     ;; Evaluate expression
     (t
      (dape--repl-insert-prompt)
      (let ((conn (or (dape--live-connection 'stopped t)
                      (dape--live-connection 'last)))
            (input (string-trim (substring-no-properties input))))
        (dape--with-request-bind
            ((&whole body &key variablesReference result &allow-other-keys) error)
            (dape--evaluate-expression conn (plist-get (dape--current-stack-frame conn) :id)
                                       input "repl")
          (cond
           (error
            (dape--repl-message error 'dape-repl-error-face))
           ((and-let* (((numberp variablesReference))
                       ((not (zerop variablesReference)))))
            (dape--repl-create-variable-table conn
                                              (plist-put body :name input)
                                              #'dape--repl-message))
           (t
            (dape--update conn 'variables nil)
            (dape--repl-message result)))))))))

(defun dape--repl-completion-at-point ()
  "Completion at point function for *dape-repl* buffer."
  (let* ((bounds (or (bounds-of-thing-at-point 'word)
                     (cons (point) (point))))
         (trigger-chars
          (when-let ((conn (or (dape--live-connection 'stopped t)
                               (dape--live-connection 'last t))))
            (or (thread-first conn
                              (dape--capabilities)
                              ;; completionTriggerCharacters is an
                              ;; unofficial array of string to trigger
                              ;; completion on.
                              (plist-get :completionTriggerCharacters)
                              (append nil))
                '("."))))
         (collection
          ;; Add `dape-repl-commands' only if completion starts at
          ;; beginning of prompt line.
          (when (eql (comint-line-beginning-position)
                     (car bounds))
            (mapcar (lambda (cmd)
                      (cons (car cmd)
                            (format " %s"
                                    (propertize (symbol-name (cdr cmd))
                                                'face 'font-lock-builtin-face))))
                    (append dape-repl-commands
                            (when dape-repl-use-shorthand
                              (dape--repl-shorthand-alist))))))
         (line-start (comint-line-beginning-position))
         (str (buffer-substring-no-properties line-start (point-max)))
         ;; Point in `str'
         (column (1+ (- (point) line-start)))
         done)
    (list
     (car bounds)
     (cdr bounds)
     (completion-table-dynamic
      (lambda (_str)
        (when-let* ((conn (or (dape--live-connection 'stopped t)
                              (dape--live-connection 'last t)))
                    ((dape--capable-p conn :supportsCompletionsRequest)))
          (dape--with-request-bind
              ((&key targets &allow-other-keys) _error)
              (dape-request conn
                            "completions"
                            (append
                             (when (dape--stopped-threads conn)
                               (list :frameId
                                     (plist-get (dape--current-stack-frame conn) :id)))
                             (list
                              :text str
                              :column column)))
            (setq collection
                  (append
                   collection
                   (mapcar
                    (lambda (target)
                      (cons
                       (or (plist-get target :text)
                           (plist-get target :label))
                       (concat (when-let ((type (plist-get target :type)))
                                 (format " %s" (propertize type 'face 'font-lock-type-face)))
                               (when-let ((detail (plist-get target :detail)))
                                 (format " %s" (propertize detail 'face 'font-lock-doc-face))))))
                    targets)))
            (setf done t))
          (while-no-input
            (while (not done)
              (accept-process-output nil 0 1))))
        collection))
     :annotation-function
     (lambda (str)
       (when-let ((annotation
                   (alist-get (substring-no-properties str) collection
                              nil nil 'equal)))
         annotation))
     :company-prefix-length
     (save-excursion
       (goto-char (car bounds))
       (looking-back (regexp-opt trigger-chars) line-start)))))

(defvar dape-repl-mode nil)

(define-derived-mode dape-repl-mode comint-mode "REPL"
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
    (insert (format
             "* Welcome to Dape REPL! *
Available Dape commands: %s
Empty input will rerun last command.\n\n"
             (mapconcat
              (pcase-lambda (`(,str . ,command))
                (setq str (concat str))
                (when dape-repl-use-shorthand
                  (set-text-properties
                   0 (thread-last (dape--repl-shorthand-alist)
                                  (rassoc command)
                                  (car)
                                  (length))
                   '(font-lock-face help-key-binding)
                   str))
                str)
              dape-repl-commands
              ", ")))
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


;;; Inlay hints

(defcustom dape-inlay-hints nil
  "Inlay variable hints."
  :type '(choice
          (const :tag "No inlay hints." nil)
          (const :tag "Inlay current line and previous line (same as 2)." t)
          (natnum :tag "Number of lines with hints.")))

(defcustom dape-inlay-hints-variable-name-max 25
  "Max length of variable name in inlay hints."
  :type 'integer)

(defface dape-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay-hint overlays.")

(defface dape-inlay-hint-highlight-face '((t (:height 0.8 :inherit (bold highlight))))
  "Face used for highlighting parts of inlay-hint overlays.")

(defvar dape--inlay-hint-overlays nil)
(defvar dape--inlay-hint-debounce-timer (timer-create))
(defvar dape--inlay-hints-symbols-fn #'dape--inlay-hint-symbols)
(defvar dape--inlay-hint-seperator (propertize " | " 'face 'dape-inlay-hint-face))

(defun dape--inlay-hint-symbols (beg end)
  "Return list of variable candidates from BEG to END."
  (unless (<= (- end beg) 300)
    ;; Sanity clamp beg and end
    (setq end (+ beg 300)))
  (save-excursion
    (goto-char beg)
    (cl-loop for symbol = (thing-at-point 'symbol)
             when
             (and symbol
                  (not (memql (get-text-property 0 'face symbol)
                              '(font-lock-string-face
                                font-lock-doc-face
                                font-lock-comment-face))))
             collect (list symbol) into symbols
             for last-point = (point)
             do (forward-thing 'symbol)
             while (and (< last-point (point))
                        (<= (point) end))
             finally return (delete-dups symbols))))

(defun dape--inlay-hint-add ()
  "Create inlay hint at current line."
  (when-let* ((ov dape--stack-position-overlay)
              (buffer (overlay-buffer ov))
              (new-head
               (with-current-buffer buffer
                 (pcase-let ((`(,beg . ,end)
                              (save-excursion
                                (goto-char (overlay-start ov))
                                (beginning-of-line)
                                (cons (point) (line-end-position)))))
                   (unless (cl-find-if (lambda (ov)
                                         (eq (overlay-get ov 'category)
                                             'dape-inlay-hint))
                                       (overlays-in beg end))
                     (let ((new-head (make-overlay beg end)))
                       (overlay-put new-head 'category 'dape-inlay-hint)
                       (overlay-put new-head 'dape-symbols
                                    (funcall dape--inlay-hints-symbols-fn
                                             beg end))
                       new-head))))))
    (setq dape--inlay-hint-overlays
          (cl-loop for inlay-hint in (cons new-head dape--inlay-hint-overlays)
                   for i from 0
                   if (< i (if (eq dape-inlay-hints t)
                               2
                             dape-inlay-hints))
                   collect inlay-hint
                   else do (delete-overlay inlay-hint)))))

(defun dape--inlay-hint-update-1 (scopes)
  "Helper for `dape--inlay-hint-update-1'.
Update `dape--inlay-hint-overlays' from SCOPES."
  (cl-loop with symbols =
           (cl-loop for inlay-hint in dape--inlay-hint-overlays
                    when (overlayp inlay-hint)
                    append (overlay-get inlay-hint 'dape-symbols))
           for scope in scopes do
           (cl-loop for variable in (plist-get scope :variables)
                    while symbols do
                    (cl-loop for name = (plist-get variable :name)
                             for cons = (assoc name symbols)
                             while cons
                             for (_ old-value) = cons
                             for value = (plist-get variable :value)
                             for updated-p = (and old-value
                                                  (not (equal old-value value)))
                             do
                             (setcdr cons (list value updated-p))
                             (setf symbols (delq cons symbols)))))
  (cl-loop
   for inlay-hint in dape--inlay-hint-overlays
   when (overlayp inlay-hint) do
   (cl-loop
    with symbols = (overlay-get inlay-hint 'dape-symbols)
    for (symbol value update) in  symbols
    when value collect
    (concat
     (propertize (format "%s:" symbol)
                 'face 'dape-inlay-hint-face
                 'mouse-face 'highlight
                 'keymap
                 (let ((map (make-sparse-keymap))
                       (sym symbol))
                   (define-key map [mouse-1]
                               (lambda (event)
                                 (interactive "e")
                                 (save-selected-window
                                   (let ((start (event-start event)))
                                     (select-window (posn-window start))
                                     (save-excursion
                                       (goto-char (posn-point start))
                                       (dape-watch-dwim sym nil t))))))
                   map)
                 'help-echo
                 (format "mouse-2: add `%s' to watch" symbol))
     " "
     (propertize (truncate-string-to-width
                  (substring value 0 (string-match-p "\n" value))
                  dape-inlay-hints-variable-name-max nil nil t)
                 'help-echo value
                 'face (if update 'dape-inlay-hint-highlight-face
                         'dape-inlay-hint-face)))
    into after-string finally do
    (thread-last (mapconcat 'identity after-string dape--inlay-hint-seperator)
                 (format "  %s")
                 (overlay-put inlay-hint 'after-string)))))

(defun dape-inlay-hints-update ()
  "Update inlay hints."
  (when-let* (((or (eq dape-inlay-hints t)
                   (and (numberp dape-inlay-hints)
                        (< 0 dape-inlay-hints))))
              (conn (dape--live-connection 'stopped t))
              (stack (dape--current-stack-frame conn))
              (scopes (plist-get stack :scopes)))
    (dape--inlay-hint-add)
    (dape--with-debounce dape--inlay-hint-debounce-timer 0.05
      (let ((responses 0))
        (dolist (scope scopes)
          (dape--with-request (dape--variables conn scope)
            (setf responses (1+ responses))
            (when (length= scopes responses)
              (dape--inlay-hint-update-1 scopes))))))))

(defun dape--inlay-hints-clean-up ()
  "Delete inlay hint overlays."
  (unless dape-active-mode
    (dolist (inlay-hint dape--inlay-hint-overlays)
      (when (overlayp inlay-hint)
        (delete-overlay inlay-hint)))
    (setq dape--inlay-hint-overlays nil)))

(add-hook 'dape-update-ui-hook #'dape-inlay-hints-update)
;; TODO Create hook for UI cleanup (restart, quit and disconnect)
(add-hook 'dape-active-mode-hook #'dape--inlay-hints-clean-up)


;;; Minibuffer config hints

(defface dape-minibuffer-hint-separator-face '((t :inherit shadow
                                                  :strike-through t))
  "Face used to separate hint overlay.")

(defvar dape--minibuffer-suggestions nil
  "Suggested configurations in minibuffer.")

(defvar dape--minibuffer-last-buffer nil
  "Helper var for `dape--minibuffer-hint'.")

(defvar dape--minibuffer-cache nil
  "Helper var for `dape--minibuffer-hint'.")

(defvar dape--minibuffer-hint-overlay nil
  "Overlay for `dape--minibuffer-hint'.")

(defun dape--minibuffer-hint (&rest _)
  "Display current configuration in minibuffer in overlay."
  (pcase-let*
      ((`(,key ,config ,error-message ,hint-rows) dape--minibuffer-cache)
       (str (string-trim (buffer-substring (minibuffer-prompt-end) (point-max))))
       (`(,hint-key ,hint-config) (ignore-errors (dape--config-from-string str t)))
       (default-directory
        (or (with-current-buffer dape--minibuffer-last-buffer
              (ignore-errors (dape--guess-root hint-config)))
            default-directory))
       (use-cache (and (equal hint-key key)
                       (equal hint-config config)))
       (use-ensure-cache
        ;; Ensure is expensive so we are cheating and don't re run
        ;; ensure if an ensure has evaled without signaling once
        (and (equal hint-key key)
             (not error-message)))
       (error-message
        (if use-ensure-cache
            error-message
          (condition-case err
              (progn (with-current-buffer dape--minibuffer-last-buffer
                       (dape--config-ensure hint-config t))
                     nil)
            (error (error-message-string err)))))
       (hint-rows
        (if use-cache
            hint-rows
          (cl-loop
           with base-config = (alist-get hint-key dape-configs)
           for (key value) on hint-config by 'cddr
           unless (or (memq key dape-minibuffer-hint-ignore-properties)
                      (and (eq key 'port) (eq value :autoport))
                      (eq key 'ensure))
           collect
           (concat
            (propertize (format "%s" key)
                        'face 'font-lock-keyword-face)
            " "
            (with-current-buffer dape--minibuffer-last-buffer
              (condition-case err
                  (propertize
                   (format "%S" (dape--config-eval-value value nil nil t))
                   'face
                   (when (equal value (plist-get base-config key))
                     'shadow))
                (error
                 (propertize (error-message-string err)
                             'face 'error)))))))))
    (setq dape--minibuffer-cache
          (list hint-key hint-config error-message hint-rows))
    (overlay-put dape--minibuffer-hint-overlay
                 'before-string
                 (concat
                  (propertize " " 'cursor 0)
                  (when error-message
                    (format "%s" (propertize error-message 'face 'error)))))
    (when dape-minibuffer-hint
      (overlay-put dape--minibuffer-hint-overlay
                   'after-string
                   (concat
                    (when hint-rows
                      (concat
                       "\n"
                       (propertize
                        " " 'face 'dape-minibuffer-hint-separator-face
                        'display '(space :align-to right))
                       "\n"
                       (mapconcat 'identity hint-rows "\n")))))
      (move-overlay dape--minibuffer-hint-overlay
                    (point-max) (point-max) (current-buffer)))))


;;; Config

(defun dape-config-get (config prop)
  "Return PROP value in CONFIG evaluated."
  (dape--config-eval-value (plist-get config prop)))

(defun dape--plistp (object)
  "Non-nil if and only if OBJECT is a valid plist."
  (and-let* (((listp object))
             (len (length object))
             ((zerop (% len 2))))))

(defun dape--config-eval-value (value &optional skip-functions check
                                      skip-interactive)
  "Return recursively evaluated VALUE.
If SKIP-FUNCTIONS is non nil return VALUE as is if `functionp' is non
nil.
If CHECK is non nil assert VALUE types, signal `user-error' on
mismatch.
If SKIP-INTERACTIVE is non nil return VALUE as is if `functionp' is
non nil and function uses the minibuffer."
  (pcase value
    ;; On function
    ((pred functionp)
     (cond
      (skip-functions value)
      (skip-interactive
       ;; Try to eval function, but escape if functions spawns an minibuffer
       (condition-case _
           (let ((enable-recursive-minibuffers nil))
             (funcall-interactively value))
         (error value)))
      (t
       (funcall-interactively value))))
    ;; On plist recursively evaluate
    ((pred dape--plistp)
     (dape--config-eval-1 value skip-functions check skip-interactive))
    ;; On vector evaluate each item
    ((pred vectorp)
     (cl-map 'vector
             (lambda (value)
               (dape--config-eval-value value
                                        skip-functions
                                        check
                                        skip-interactive))
             value))
    ;; On symbol evaluate symbol value
    ((and (pred symbolp)
          ;; Guard against infinite recursion
          (guard (not (eq (symbol-value value) value))))
     (dape--config-eval-value (symbol-value value)
                              skip-functions check skip-interactive))
    ;; Otherwise return value
    (_ value)))

(defun dape--config-eval-1 (config &optional skip-functions check
                                   skip-interactive)
  "Helper for `dape--config-eval'."
  (cl-loop for (key value) on config by 'cddr append
           (cond
            ((memql key '(modes fn ensure)) (list key value))
            ((and check (not (keywordp key)))
             (user-error "Unexpected key %S; lists of things needs be \
arrays [%S ...], if meant as an object replace (%S ...) with (:%s ...)"
                         key key key key))
            ((list key
                   (dape--config-eval-value value skip-functions
                                            (or check (keywordp key))
                                            skip-interactive))))))
(defun dape--config-eval (key options)
  "Evaluate config with KEY and OPTIONS."
  (let ((base-config (alist-get key dape-configs)))
    (unless base-config
      (user-error "Unable to find `%s' in `dape-configs', available \
configurations: %s"
                  key (mapconcat (lambda (e) (symbol-name (car e)))
                                 dape-configs ", ")))
    (dape--config-eval-1 (seq-reduce (apply-partially 'apply 'plist-put)
                                     (seq-partition options 2)
                                     (copy-tree base-config)))))

(defun dape--config-from-string (str &optional loose-parsing)
  "Return list of ALIST-KEY CONFIG from STR.
Expects STR format of \”ALIST-KEY PLIST-KEY PLIST-VALUE\” etc.
Where ALIST-KEY exists in `dape-configs'.
If LOOSE-PARSING is non nil ignore arg parsing failures."
  (let ((buffer (current-buffer))
        name read-config base-config)
    (with-temp-buffer
      ;; Keep possible local `dape-configs' value
      (setq-local dape-configs
                  (buffer-local-value 'dape-configs buffer))
      (insert str)
      (goto-char (point-min))
      (unless (setq name (ignore-errors (read (current-buffer))))
        (user-error "Expects config name (%s)"
                    (mapconcat (lambda (e) (symbol-name (car e)))
                               dape-configs ", ")))
      (unless (alist-get name dape-configs)
        (user-error "No configuration named `%s'" name))
      (setq base-config (copy-tree (alist-get name dape-configs)))
      (condition-case _
          (while
              ;; Do we have non whitespace chars after `point'?
              (thread-first (buffer-substring (point) (point-max))
                            (string-trim)
                            (string-empty-p)
                            (not))
            (let ((thing (read (current-buffer))))
              (cond
               ((eq thing '-)
                (cl-loop
                 with command = (split-string-shell-command
                                 (buffer-substring (point) (point-max)))
                 with setvar = "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)\\'"
                 for cell on command for (program . args) = cell
                 when (string-match setvar program)
                 append `(,(intern (concat ":" (match-string 1 program)))
                          ,(match-string 2 program))
                 into env and do (setq program nil)
                 when (or (and (not program) (not args)) program) do
                 (setq read-config
                       (append (nreverse
                                (append (when program `(:program ,program))
                                        (when args `(:args ,(apply 'vector args)))
                                        (when env `(:env ,env))))
                               read-config))
                 (throw 'done nil)))
               (t
                (push thing read-config)))))
        (error
         (unless loose-parsing
           (user-error "Unable to parse options %s"
                       (buffer-substring (point) (point-max)))))))
    (when (and loose-parsing
               (not (dape--plistp read-config)))
      (pop read-config))
    (setq read-config (nreverse read-config))
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
                         ;; Has value changed (skip functions)?
                         (equal (dape--config-eval-value
                                 (plist-get base-config key)
                                 'skip-functions)
                                value)))
             append (list key value))))

(defun dape--config-to-string (key post-eval-config)
  "Create string from KEY and POST-EVAL-CONFIG."
  (let ((config-diff (dape--config-diff key post-eval-config)))
    (concat (when key (format "%s" key))
            (and-let* ((config-diff)
                       (config-str (prin1-to-string config-diff)))
              (format " %s"
                      (substring config-str 1 (1- (length config-str))))))))

(defun dape--config-ensure (config &optional signal)
  "Ensure that CONFIG is executable.
If SIGNAL is non nil raises `user-error' on failure otherwise returns
nil."
  (if-let ((ensure-fn (plist-get config 'ensure)))
      (let ((default-directory
             (or (when-let ((command-cwd (plist-get config 'command-cwd)))
                   (dape--config-eval-value command-cwd))
                 default-directory)))
        (condition-case err
            (or (funcall ensure-fn config) t)
          (error
           (if signal (user-error (error-message-string err)) nil))))
    t))

(defun dape--config-mode-p (config)
  "Is CONFIG enabled for current mode."
  (let ((modes (plist-get config 'modes)))
    (or (not modes)
        (apply 'provided-mode-derived-p
               major-mode (cl-map 'list 'identity modes))
        (and-let* (((not (derived-mode-p 'prog-mode)))
                   (last-hist (car dape-history))
                   (last-config (cadr (dape--config-from-string last-hist))))
          (cl-some (lambda (mode)
                     (memql mode (plist-get last-config 'modes)))
                   modes)))))

(defun dape--config-completion-at-point ()
  "Function for `completion-at-point' fn for `dape--read-config'."
  (let (key args args-bounds last-p)
    (save-excursion
      (goto-char (minibuffer-prompt-end))
      (setq key (ignore-errors (read (current-buffer))))
      (ignore-errors
        (while t
          (setq last-p (point))
          (push (read (current-buffer)) args)
          (push (cons last-p (point)) args-bounds))))
    (setq args (nreverse args)
          args-bounds (nreverse args-bounds))
    (cond
     ;; Complete config key
     ((or (not key)
          (and (not args)
               (thing-at-point 'symbol)))
      (pcase-let ((`(,start . ,end)
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
        (list start end
              (mapcar (lambda (suggestion) (format "%s " suggestion))
                      dape--minibuffer-suggestions))))
     ;; Complete config args
     ((and (not (plist-member args '-))
           (alist-get key dape-configs)
           (or (and (plistp args)
                    (thing-at-point 'whitespace))
               (cl-loop with p = (point)
                        for ((start . end) _) on args-bounds by 'cddr
                        when (and (<= start p) (<= p end))
                        return t
                        finally return nil)))
      (pcase-let ((`(,start . ,end)
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
        (list start end
              (cl-loop with plist = (append (alist-get key dape-configs)
                                            '(compile nil))
                       for (key _) on plist by 'cddr
                       collect (format "%s " key)))))
     (t
      (list (point) (point) nil :exclusive 'no)))))

(defun dape--read-config ()
  "Read config from minibuffer.
Initial contents defaults to valid configuration if there is only one
or last mode valid history item from this session.

See `dape--config-mode-p' how \"valid\" is defined."
  (let* ((suggested-configs
          (cl-loop for (key . config) in dape-configs
                   when (and (dape--config-mode-p config)
                             (dape--config-ensure config))
                   collect (dape--config-to-string key nil)))
         (initial-contents
          (or
           ;; Take `dape-command' if exist
           (when dape-command
             (dape--config-to-string (car dape-command)
                                     (cdr dape-command)))
           ;; Take first valid history item
           (seq-find (lambda (str)
                       (ignore-errors
                         (member (thread-first (dape--config-from-string str)
                                               (car)
                                               (dape--config-to-string nil))
                                 suggested-configs)))
                     dape-history)
           ;; Take first suggested config if only one exist
           (and (length= suggested-configs 1)
                (car suggested-configs))))
         (default-value (when initial-contents
                          (concat (car (string-split initial-contents)) " "))))
    (setq dape--minibuffer-last-buffer (current-buffer)
          dape--minibuffer-cache nil)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local dape--minibuffer-suggestions suggested-configs
                      comint-completion-addsuffix nil
                      resize-mini-windows t
                      max-mini-window-height 0.5
                      dape--minibuffer-hint-overlay (make-overlay (point) (point))
                      default-directory (dape-command-cwd)
                      ;; Store origin buffer `dape-configs' value
                      dape-configs (buffer-local-value
                                    'dape-configs dape--minibuffer-last-buffer))
          (set-syntax-table emacs-lisp-mode-syntax-table)
          (add-hook 'completion-at-point-functions
                    'comint-filename-completion nil t)
          (add-hook 'completion-at-point-functions
                    #'dape--config-completion-at-point nil t)
          (add-hook 'after-change-functions
                    #'dape--minibuffer-hint nil t)
          (dape--minibuffer-hint))
      (pcase-let*
          ((str
            (let ((history-add-new-input nil))
              (read-from-minibuffer
               "Run adapter: "
               initial-contents
               (let ((map (make-sparse-keymap)))
                 (set-keymap-parent map minibuffer-local-map)
                 (define-key map (kbd "C-M-i") #'completion-at-point)
                 (define-key map "\t" #'completion-at-point)
                 ;; This mapping is shadowed by `next-history-element'
                 ;; future history (default-value)
                 (define-key map (kbd "C-c C-k")
                             (lambda ()
                               (interactive)
                               (pcase-let*
                                   ((str (buffer-substring (minibuffer-prompt-end)
                                                           (point-max)))
                                    (`(,key) (dape--config-from-string str t)))
                                 (delete-region (minibuffer-prompt-end)
                                                (point-max))
                                 (insert (format "%s" key) " "))))
                 map)
               nil 'dape-history default-value)))
           (`(,key ,config)
            (dape--config-from-string (substring-no-properties str) t))
           (evaled-config (dape--config-eval key config)))
        (setq dape-history
              (cons (dape--config-to-string key evaled-config)
                    dape-history))
        evaled-config))))


;;; Hover

(defun dape-hover-function (cb)
  "Hook function to produce doc strings for `eldoc'.
On success calls CB with the doc string.
See `eldoc-documentation-functions', for more information."
  (and-let* ((conn (dape--live-connection 'last t))
             ((dape--capable-p conn :supportsEvaluateForHovers))
             (symbol (thing-at-point 'symbol)))
    (dape--with-request-bind
        (body error)
        (dape--evaluate-expression
         conn
         (plist-get (dape--current-stack-frame conn) :id)
         (substring-no-properties symbol)
         "hover")
      (unless error
        (funcall cb
                 (format "%s %s"
                         (or (plist-get body :value)
                             (plist-get body :result)
                             "")
                         (propertize
                          (or (plist-get body :type) "")
                          'face 'font-lock-type-face))
                 :thing symbol
                 :face 'font-lock-variable-name-face))))
  t)

(defun dape--add-eldoc-hook ()
  "Add `dape-hover-function' from eldoc hook."
  (add-hook 'eldoc-documentation-functions #'dape-hover-function nil t))

(defun dape--remove-eldoc-hook ()
  "Remove `dape-hover-function' from eldoc hook."
  (remove-hook 'eldoc-documentation-functions #'dape-hover-function t))


;;; Mode line

(easy-menu-define dape-menu nil
  "Menu for `dape-active-mode'."
  `("Dape"
    ["Continue" dape-continue :enable (dape--live-connection 'stopped)]
    ["Next" dape-next :enable (dape--live-connection 'stopped)]
    ["Step in" dape-step-in :enable (dape--live-connection 'stopped)]
    ["Step out" dape-step-out :enable (dape--live-connection 'stopped)]
    ["Pause" dape-pause :enable (not (dape--live-connection 'stopped t))]
    ["Quit" dape-quit]
    "--"
    ["REPL" dape-repl]
    ["Info buffers" dape-info]
    ["Memory" dape-read-memory
     :enable (dape--capable-p (dape--live-connection 'last)
                              :supportsReadMemoryRequest)]
    "--"
    ["Customize Dape" (lambda () (interactive) (customize-group "dape"))]))

(defun dape--update-state (conn state &optional reason)
  "Update Dape mode line with STATE symbol for adapter CONN."
  (setf (dape--state conn) state)
  (setf (dape--state-reason conn) reason)
  (dape--mode-line-format)
  (force-mode-line-update t))

(defvar dape--mode-line-format nil
  "Dape mode line format.")

(put 'dape--mode-line-format 'risky-local-variable t)

(defun dape--mode-line-format ()
  "Update `dape--mode-line-format' format."
  (let ((conn (or (dape--live-connection 'last t)
                  dape--connection)))
    (setq dape--mode-line-format
          `((:propertize "dape"
                         face font-lock-constant-face
                         mouse-face mode-line-highlight
                         help-echo "Dape: Debug Adapter Protocol for Emacs\n\
mouse-1: Display minor mode menu"
                         keymap ,(let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line down-mouse-1] dape-menu)
                                   map))
            ":"
            (:propertize ,(format "%s" (or (and conn (dape--state conn))
                                           'unknown))
                         face font-lock-doc-face)
            ,@(when-let ((reason (and conn (dape--state-reason conn))))
                `("/" (:propertize ,reason face font-lock-doc-face)))
            ,@(when-let* ((conns (dape--live-connections))
                          (nof-conns
                           (length (cl-remove-if-not 'dape--threads conns)))
                          ((> nof-conns 1)))
                `((:propertize ,(format "(%s)" nof-conns)
                               face shadow
                               help-echo "Active child connections")))))))

(add-to-list 'mode-line-misc-info
             `(dape-active-mode ("[" dape--mode-line-format "]")))


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
    (define-key map "h" #'dape-breakpoint-hits)
    (define-key map "b" #'dape-breakpoint-toggle)
    (define-key map "B" #'dape-breakpoint-remove-all)
    (define-key map "t" #'dape-select-thread)
    (define-key map "S" #'dape-select-stack)
    (define-key map ">" #'dape-stack-select-down)
    (define-key map "<" #'dape-stack-select-up)
    (define-key map "x" #'dape-evaluate-expression)
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
               dape-breakpoint-hits
               dape-breakpoint-toggle
               dape-breakpoint-remove-all
               dape-stack-select-up
               dape-stack-select-down
               dape-select-stack
               dape-select-thread
               dape-watch-dwim
               dape-evaluate-expression))
  (put cmd 'repeat-map 'dape-global-map))

(when dape-key-prefix (global-set-key dape-key-prefix dape-global-map))


;;; Hooks

(defun dape--kill-busy-wait ()
  "Kill connection and wait until finished."
  (let (done)
    (dape--with-request (dape-kill dape--connection)
      (setf done t))
    ;; Busy wait for response at least 2 seconds
    (cl-loop with max-iterations = 20
             for i from 1 to max-iterations
             until done
             do (accept-process-output nil 0.1))))

;; Cleanup conn before bed time
(add-hook 'kill-emacs-hook #'dape--kill-busy-wait)

(provide 'dape)

;;; dape.el ends here
