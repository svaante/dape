;;; dape.el --- Debug Adapter Protocol for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.24.1
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
(require 'project)
(require 'gdb-mi)
(require 'hexl)
(require 'tramp)
(require 'jsonrpc)


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
           :program (file-name-concat "target" "debug"
                                      (car (last (file-name-split
                                                  (directory-file-name (dape-cwd))))))
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
                           (unless (zerop (process-file-shell-command
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
                          :module (car (last (file-name-split
                                              (directory-file-name default-directory))))
                          ,@common)))
    (dlv
     modes (go-mode go-ts-mode)
     ensure dape-ensure-command
     command "dlv"
     command-args ("dap" "--listen" "127.0.0.1::autoport")
     command-cwd dape-command-cwd
     command-insert-stderr t
     port :autoport
     :request "launch"
     :type "go"
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
     :request "launch"
     :program "a.out"
     :args []
     :stopAtBeginningOfMainSubprogram nil)
    (godot
     modes (gdscript-mode)
     port 6006
     :request "launch"
     :type "server")
    ,@(let ((js-debug
             `( ensure ,(lambda (config)
                          (dape-ensure-command config)
                          (when-let* ((runtime-executable
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
          (js-debug-tsx
           modes (typescript-mode typescript-ts-mode)
           ,@js-debug
           :type "pwa-node"
           :runtimeExecutable "tsx"
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
             `( modes ( c-mode c-ts-mode
                        c++-mode c++-ts-mode
                        rust-mode rust-ts-mode rustic-mode)
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
     :program (if-let* ((dlls
                         (file-expand-wildcards
                          (file-name-concat "bin" "Debug" "*" "*.dll"))))
                  (file-relative-name (file-relative-name (car dlls)))
                ".dll")
     :stopAtEntry nil)
    (ocamlearlybird
     ensure dape-ensure-command
     modes (tuareg-mode caml-mode)
     command "ocamlearlybird"
     command-args ("debug")
     :type "ocaml"
     :program (file-name-concat (dape-cwd) "_build" "default" "bin"
                                (concat (file-name-base (dape-buffer-default)) ".bc"))
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
                               (if (eq arg :-c) (plist-get config '-c) arg))
                             (plist-get config 'command-args))))
     port :autoport
     command-cwd dape-command-cwd
     :type "Ruby"
     ;; -- examples:
     ;; rails server
     ;; bundle exec ruby foo.rb
     ;; bundle exec rake test
     -c (concat "ruby " (dape-buffer-default)))
    (jdtls
     modes (java-mode java-ts-mode)
     ensure (lambda (config)
              (let ((file (dape-config-get config :filePath)))
                (unless (and (stringp file) (file-exists-p file))
                  (user-error "Unable to locate :filePath `%s'" file))
                (with-current-buffer (find-file-noselect file)
                  (unless (and (featurep 'eglot) (eglot-current-server))
                    (user-error "No eglot instance active in buffer %s" (current-buffer)))
                  (unless (seq-contains-p (eglot--server-capable :executeCommandProvider :commands)
        			          "vscode.java.resolveClasspath")
        	    (user-error "Jdtls instance does not bundle java-debug-server, please install")))))
     fn (lambda (config)
          (with-current-buffer
              (find-file-noselect (dape-config-get config :filePath))
            (if-let* ((server (eglot-current-server)))
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
                             (with-no-warnings
                               (eglot-execute-command
                                (eglot-current-server)
                                "vscode.java.resolveMainClass"
                                (file-name-nondirectory
                                 (directory-file-name (dape-cwd))))))
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
           ,(lambda () (resolve-main-class :mainClass))
           :projectName
           ,(lambda () (resolve-main-class :projectName))))
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
- command-env: Property list (plist) of environment variables to
  set when running the command.  Keys can be strings, symbols or
  keywords.
- command-insert-stderr: If non nil treat stderr from adapter as
  stderr output from debugged program.
- prefix-local: Path prefix for Emacs file access.
- prefix-remote: Path prefix for debugger file access.
- host: Host of the debug adapter.
- port: Port of the debug adapter.
- modes: List of modes where the configuration is active in `dape'
  completions.
- compile: Executes a shell command with `dape-compile-function'.
- defer-launch-attach: If launch/attach request should be sent
  after initialize or configurationDone.  If nil launch/attach are
  sent after initialize request else it's sent after
  configurationDone.  This key exist to accommodate the two different
  interpretations of the DAP specification.
  See: GDB bug 32090.

Note: The char - carries special meaning when reading options in
`dape' and therefore should not be used be used as an key.
See `dape-history-add'.

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
                        ((const :tag "List of environment variables to set when running the command" command-env)
                         (plist :key-type (restricted-sexp :match-alternatives (stringp symbolp keywordp) :tag "Variable")
                                :value-type (string :tag "Value")))
                        ((const :tag "Treat stderr from adapter as program output" command-insert-stderr) boolean)
                        ((const :tag "Working directory for command" command-cwd) (choice string symbol))
                        ((const :tag "Path prefix for Emacs file access" prefix-local) string)
                        ((const :tag "Path prefix for debugger file access" prefix-remote) string)
                        ((const :tag "Host of debug adapter" host) string)
                        ((const :tag "Port of debug adapter" port) natnum)
                        ((const :tag "Compile cmd" compile) string)
                        ((const :tag "Use configurationDone as trigger for launch/attach" defer-launch-attach) boolean)
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
\(launch :program \"a.out\")"
  :type 'sexp)
;;;###autoload(put 'dape-command 'safe-local-variable #'listp)

(defcustom dape-key-prefix "\C-x\C-a"
  "Prefix of all dape commands."
  :type 'key-sequence)

(define-obsolete-variable-alias 'dape-buffer-window-arrangment 'dape-buffer-window-arrangement "0.3.0")
(defcustom dape-buffer-window-arrangement 'left
  "Rules for display dape buffers."
  :type '(choice (const :tag "GUD gdb like" gud)
                 (const :tag "Left side" left)
                 (const :tag "Right side" right)
                 (const :tag "Use `display-buffer-base-action'" nil)))

(defcustom dape-variable-auto-expand-alist '((hover . 1) (repl . 0) (watch . 1))
  "Default expansion depth for displaying variables.
Each entry consists of a context (such as `hover', `repl', or
`watch') paired with a number indicating how many levels deep the
variable should be expanded by default."
  :type '(alist :key-type
                (choice (natnum :tag "Scope number (Locals 0 etc.)")
                        (const :tag "Eldoc hover" hover)
                        (const :tag "In repl buffer" repl)
                        (const :tag "In watch buffer" watch)
                        (const :tag "All contexts" nil))
                :value-type (natnum :tag "Levels expanded")))

(defcustom dape-stepping-granularity 'line
  "The granularity of one step in the stepping requests."
  :type '(choice (const :tag "Step statement" statement)
                 (const :tag "Step line" line)
                 (const :tag "Step instruction" instruction)))

(defcustom dape-stack-trace-levels 20
  "The number of stack frames fetched."
  :type 'natnum)

(defcustom dape-display-source-buffer-action
  `((display-buffer-reuse-window
     display-buffer-same-window
     display-buffer-use-some-window))
  "`display-buffer' action used when displaying source buffer."
  :type 'sexp)

(define-obsolete-variable-alias 'dape-on-start-hooks 'dape-start-hook "0.13.0")
(defcustom dape-start-hook '(dape-repl dape-info)
  "Called when session starts."
  :type 'hook)

(define-obsolete-variable-alias 'dape-on-stopped-hooks 'dape-stopped-hook "0.13.0")
(defcustom dape-stopped-hook '( dape-memory-revert dape-disassemble-revert
                                dape--emacs-grab-focus)
  "Called when session stopped."
  :type 'hook)

(define-obsolete-variable-alias 'dape-update-ui-hooks 'dape-update-ui-hook "0.13.0")
(defcustom dape-update-ui-hook '(dape-info-update)
  "Called when it's sensible to refresh UI."
  :type 'hook)

(defcustom dape-display-source-hook '()
  "Called in buffer when placing overlay arrow for stack frame."
  :type 'hook)

(defcustom dape-mime-mode-alist '(("text/x-lldb.disassembly" . asm-mode)
                                  ("text/javascript" . js-mode))
  "Alist of MIME types vs corresponding major mode functions.
Each element should look like (MIME-TYPE . MODE) where MIME-TYPE is
a string and MODE is the major mode function to use for buffers of
this MIME type."
  :type '(alist :key-type string :value-type function))

(define-obsolete-variable-alias 'dape-read-memory-default-count 'dape-memory-page-size "0.8.0")
(defcustom dape-memory-page-size 1024
  "The bytes read with `dape-memory'."
  :type 'natnum)

(defcustom dape-info-buffer-window-groups
  '((dape-info-scope-mode dape-info-watch-mode)
    (dape-info-stack-mode dape-info-modules-mode dape-info-sources-mode)
    (dape-info-breakpoints-mode dape-info-threads-mode))
  "Window grouping rules for `dape-info' buffers.
Each list of MODEs is displayed in the same window.  The first item of
each group is displayed by `dape-info'.  MODE can also be
\(`dape-info-scope-mode' INDEX), displaying scope at INDEX.
All modes need not to be present in an group."
  :type '(repeat (repeat (choice
                          (function :tag "Info mode")
                          (list :tag "Scope index" (const dape-info-scope-mode)
                                (natnum :tag "Index"))))))

(defcustom dape-info-hide-mode-line
  (and (memql dape-buffer-window-arrangement '(left right)) t)
  "Hide mode line in dape info buffers."
  :type 'boolean)

(defcustom dape-info-variable-table-aligned nil
  "Align columns in variable tables."
  :type 'boolean)

(defcustom dape-info-variable-table-row-config
  `((name . 0) (value . 0) (type . 0))
  "Configuration for table rows of variables.

An ALIST that controls the display of the name, type and value of
variables.  The key controls which column to change whereas the
value determines the maximum number of characters to display in each
column.  A value of 0 means there is no limit.

Additionally, the order the element in the ALIST determines the
left-to-right display order of the properties."
  :type '(alist :key-type
                (choice (const :tag "Name" name)
                        (const :tag "Value" value)
                        (const :tag "Type" type))
                :value-type (choice (const :tag "Full" 0)
                                    (natnum :tag "Width"))))

(defcustom dape-info-thread-buffer-locations t
  "Show file information or library names in threads buffer."
  :type 'boolean)

(defcustom dape-info-thread-buffer-addresses nil
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

(defcustom dape-info-file-name-max 25
  "Max length of file name in dape info buffers."
  :type 'integer)

(defcustom dape-inlay-hints t
  "Inlay variable hints."
  :type '(choice (const :tag "No inlay hints." nil)
                 (const :tag "Inlay current line and previous line (same as 2)." t)
                 (natnum :tag "Number of lines with hints.")))

(defcustom dape-inlay-hints-variable-name-max 25
  "Max length of variable name in inlay hints."
  :type 'integer)

(defcustom dape-repl-echo-shell-output nil
  "Echo dape shell output in repl."
  :type 'boolean)

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
    ("threads" . dape-repl-threads)
    ("stack" . dape-repl-stack)
    ("modules" . dape-repl-modules)
    ("sources" . dape-repl-sources)
    ("breakpoints" . dape-repl-breakpoints)
    ("scope" . dape-repl-scope)
    ("watch" . dape-repl-watch)
    ("restart" . dape-restart)
    ("kill" . dape-kill)
    ("disconnect" . dape-disconnect-quit)
    ("quit" . dape-quit))
  "Dape commands available in *dape-repl* buffer."
  :type '(alist :key-type string :value-type function))

(defcustom dape-breakpoint-margin-string "B"
  "String to display breakpoint in margin."
  :type 'string)

(defcustom dape-default-breakpoints-file
  (locate-user-emacs-file "dape-breakpoints")
  "Default file for loading and saving breakpoints.
See `dape-breakpoint-load' and `dape-breakpoint-save'."
  :type 'file)

(define-obsolete-variable-alias 'dape-compile-fn 'dape-compile-function "0.21.0")
(defcustom dape-compile-function #'compile
  "Function to compile with.
The function is called with a command string."
  :type 'function)

(define-obsolete-variable-alias 'dape-cwd-fn 'dape-cwd-function "0.21.0")
(defcustom dape-cwd-function #'dape--default-cwd
  "Function to get current working directory.
The function should return a string representing the absolute
file path of the current working directory, usually the current
project's root.  See `dape--default-cwd'."
  :type 'function)

(define-obsolete-variable-alias 'dape-compile-compile-hooks 'dape-compile-hook "0.13.0")
(defcustom dape-compile-hook nil
  "Called after dape compilation finishes.
The hook is run with one argument, the compilation buffer when
compilation is successful."
  :type 'hook)

(defcustom dape-minibuffer-hint t
  "Show `dape-configs' hints in minibuffer."
  :type 'boolean)

(defcustom dape-minibuffer-hint-ignore-properties
  '( ensure fn modes command command-args command-env command-insert-stderr
     defer-launch-attach :type :request)
  "Properties to be ignored in minibuffer \"Run adapter\" hints.
See `dape-minibuffer-hint'."
  :type '(repeat symbol))

(defcustom dape-history-add 'input
  "How to push configuration options onto `dape-history'.

- input: Store input as it is read from the minibuffer.
- expanded: Each key in the input is evaluated, and only options that
  differ from the base configuration in `dape-configs' are stored.
- shell-like: Like expanded, but stores options in a shell-like
  format.  Characters after - are interpreted in a shell-style format,
  with ENV, PROGRAM, and ARGS.  Useful for adapters that accept :env,
  :program, and :args as launch options.
  Example: \"launch - ENV=value program arg1 arg2\"."
  :type '(choice (const :tag "Input" input)
		 (const :tag "After evaluation of each key" expanded)
		 (const :tag "Shell like with - separator" shell-like)))

(defcustom dape-ui-debounce-time 0.1
  "Number of seconds to debounce `revert-buffer' for UI buffers."
  :type 'float)

(defcustom dape-request-timeout jsonrpc-default-request-timeout
  "Number of seconds until a request is deemed to be timed out."
  :type 'natnum)

(defcustom dape-debug nil
  "If non nil log debug info in repl and connection events buffers.
Debug logging has an noticeable effect on performance."
  :type 'boolean)


;;; Face
(defface dape-breakpoint-face '((t :inherit font-lock-keyword-face))
  "Face used to display breakpoint overlays.")

(defface dape-breakpoint-until-face '((t :inherit font-lock-doc-face))
  "Face used to display until breakpoint overlays.")

(defface dape-log-face '((t :inherit dape-breakpoint-face
                            :height 0.85 :box (:line-width -1)))
  "Face used to display log breakpoints.")

(defface dape-expression-face '((t :inherit dape-breakpoint-face
                                   :height 0.85 :box (:line-width -1)))
  "Face used to display conditional breakpoints.")

(defface dape-hits-face '((t :inherit dape-breakpoint-face
                             :height 0.85 :box (:line-width -1)))
  "Face used to display hits breakpoints.")

(defface dape-exception-description-face '((t :inherit (error tooltip)
                                              :extend t))
  "Face used to display exception descriptions inline.")

(defface dape-source-line-face '((t))
  "Face used to display stack frame source line overlays.")

(defface dape-repl-error-face '((t :inherit compilation-mode-line-fail
                                   :extend t))
  "Face used in repl for non 0 exit codes.")


;;; Forward declarations
(defvar hl-line-mode)
(defvar hl-line-sticky-flag)
(declare-function global-hl-line-highlight  "hl-line" ())
(declare-function hl-line-highlight         "hl-line" ())


;;; Vars

(defvar dape-history nil
  "History variable for `dape'.")

;; FIXME `dape--source-buffers' should be moved into connection as
;; source references are not globally scoped.
(defvar dape--source-buffers nil
  "Plist of sources reference to buffer.")
(defvar dape--breakpoints nil
  "List of `dape--breakpoint's.")
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

(define-minor-mode dape-active-mode
  "On when dape debugging session is active.
Non interactive global minor mode."
  :global t
  :interactive nil)


;;; Utils

(defun dape--warn (format &rest args)
  "Display warning/error message with FORMAT and ARGS."
  (dape--repl-insert-error (format "* %s *\n" (apply #'format format args))))

(defun dape--message (format &rest args)
  "Display message with FORMAT and ARGS."
  (dape--repl-insert (format "* %s *\n" (apply #'format format args))))

(defmacro dape--with-request-bind (vars fn-args &rest body)
  "Call FN with ARGS and execute BODY on callback with VARS bound.
VARS are bound from the arguments that the callback is invoked
with.  FN-ARGS is a list of (FN . ARGS).  FN is called with ARGS
followed by a callback function.  BODY is evaluated in the buffer that
was active when this macro was invoked.  If that buffer is no longer
live, BODY is evaluated in the buffer current at callback execution
time.
See `cl-destructuring-bind' for details on valid bind forms for
VARS."
  (declare (indent 2))
  (let ((old-buffer (make-symbol "old-buffer")))
    `(let ((,old-buffer (current-buffer)))
       (,(car fn-args) ,@(cdr fn-args)
        (cl-function
         (lambda ,vars
           (with-current-buffer (if (buffer-live-p ,old-buffer)
                                    ,old-buffer
                                  (current-buffer))
             ,@body)))))))

(defmacro dape--with-request (fn-args &rest body)
  "Call `dape-request' like FN with ARGS and execute BODY on callback.
FN-ARGS is be an cons pair as FN . ARGS.
BODY is guaranteed to be evaluated with the current buffer if live.
See `cl-destructuring-bind' for bind forms."
  (declare (indent 1))
  `(dape--with-request-bind (&rest _) ,fn-args ,@body))

(defun dape--request-continue (cb &optional error)
  "Shorthand to call CB with ERROR in an `dape-request' like way."
  (when (functionp cb)
    (funcall cb nil error)))

(defun dape--call-with-debounce (timer backoff fn)
  "Call FN with a debounce of BACKOFF seconds.
This function utilizes TIMER to store state.  It cancels the TIMER
and schedules FN to run after current time + BACKOFF seconds.
If BACKOFF is non-zero, FN will be evaluated within timer context."
  (cond ((zerop backoff)
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

(defmacro dape--with-line (buffer line &rest body)
  "Save point and buffer then execute BODY on LINE in BUFFER."
  (declare (indent 2))
  `(with-current-buffer ,buffer
     (save-excursion
       (goto-char (point-min))
       (forward-line (1- ,line))
       ,@body)))

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

(defun dape--maybe-select-thread (conn thread-id &optional force)
  "Maybe set selected THREAD-ID and CONN.
If FORCE is non nil, force selection of the thread.
If the thread is selected, also select CONN if no connection has been
selected yet, or if the currently selected connection has no stopped
threads.
See `dape--connection-selected'."
  (when (and thread-id (or force (not (dape--thread-id conn))))
    (setf (dape--thread-id conn) thread-id)
    ;; Update selected connection if the current one is not live or
    ;; has no stopped threads.
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
    (cond ((not status) nil)
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
  (when-let* ((thread-id (dape--thread-id conn)))
    (list :threadId thread-id)))

(defun dape--stopped-threads (conn)
  "List of stopped threads for CONN."
  (when conn
    (mapcan (lambda (thread)
              (when (equal (plist-get thread :status) 'stopped)
                (list thread)))
            (dape--threads conn))))

(defun dape--current-thread (conn)
  "Current thread plist for CONN."
  (when conn
    (cl-find-if (lambda (thread)
                  (eq (plist-get thread :id) (dape--thread-id conn)))
                (dape--threads conn))))

(defun dape--path-1 (conn path format)
  "Return translate absolute PATH in FORMAT from CONN config.
Accepted FORMAT values are local and remote.
See `dape-configs' symbols prefix-local prefix-remote."
  (if-let* (;; Fallback to last connection
            (config (dape--config (or conn dape--connection)))
            ;; If neither `prefix-local' or `prefix-remote' is set there is
            ;; no work to be done
            ((or (plist-member config 'prefix-local)
                 (plist-member config 'prefix-remote)))
            (absolute-path
             ;; `command-cwd' is always set in `dape--launch-or-attach'
             (let ((command-cwd (plist-get config 'command-cwd)))
               (expand-file-name path
                                 (pcase format
                                   ('local (tramp-file-local-name command-cwd))
                                   ('remote command-cwd)))))
            (prefix-local (or (plist-get config 'prefix-local) ""))
            (prefix-remote (or (plist-get config 'prefix-remote) ""))
            (mapping (pcase format
                       ('local (cons prefix-remote prefix-local))
                       ('remote (cons prefix-local prefix-remote))
                       (_ (error "Unknown format"))))
            ;; Substitute prefix if there is an match or nil
            ((string-prefix-p (car mapping) absolute-path)))
      (concat (cdr mapping)
              (string-remove-prefix (car mapping) absolute-path))
    path))

(defun dape--path-local (conn path)
  "Return translate PATH to local format by CONN.
See `dape--path-1'."
  (dape--path-1 conn path 'local))

(defun dape--path-remote (conn path)
  "Return translate PATH to remote format by CONN.
See `dape--path-1'."
  (dape--path-1 conn path 'remote))

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
  (when-let* ((source (plist-get plist :source))
              (line (or (plist-get plist :line) 1))
              (buffer (or
                       ;; Take buffer by source reference
                       (when-let* ((reference (plist-get source :sourceReference))
                                   (buffer (plist-get dape--source-buffers reference))
                                   ((buffer-live-p buffer)))
                         buffer)
                       ;; Take buffer by path
                       (when-let* ((remote-path (plist-get source :path))
                                   (path (dape--path-local conn remote-path))
                                   ((file-exists-p path)))
                         (find-file-noselect path t)))))
    (dape--with-line buffer line
      (when-let* ((column (plist-get plist :column)))
        (when (> column 0)
          (forward-char (1- column))))
      (point-marker))))

(defun dape--default-cwd ()
  "Try to guess current project absolute file path with `project'."
  (or (when-let* ((project (project-current)))
        (expand-file-name (project-root project)))
      default-directory))

(defun dape-cwd ()
  "Use `dape-cwd-function' to guess current working as local path."
  (tramp-file-local-name (funcall dape-cwd-function)))

(defun dape-command-cwd ()
  "Use `dape-cwd-function' to guess current working directory."
  (funcall dape-cwd-function))

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
  "Handle :autoport in CONFIG keys `port', `command-args', and `command-env'.
If `port' is the symbol `:autoport', replace it with a random free port
number.  In addition, replace all occurences of `:autoport' (symbol or
string) in `command-args' and all property values of `command-env' with
the value of config key `port'."
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
  (when-let* ((port (plist-get config 'port))
              (port-string (number-to-string port))
              (replace-fn (lambda (arg)
                            (cond
                             ((eq arg :autoport) port-string)
                             ((stringp arg) (string-replace ":autoport" port-string arg))
                             (t arg)))))
    (when-let* ((command-args (plist-get config 'command-args)))
      (plist-put config 'command-args (seq-map replace-fn command-args)))
    (when-let* ((command-env (plist-get config 'command-env)))
      (plist-put config 'command-env
                 (cl-loop for (key value) on command-env by #'cddr
                          collect key
                          collect (apply replace-fn (list value))))))
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
        (dape--message "Remote connection detected, setting prefix-local to %S"
                       prefix-local)
        (plist-put config 'prefix-local prefix-local)))
    (when (and (plist-get config 'command)
               (plist-get config 'port)
               (not (plist-get config 'host))
               (equal (tramp-file-name-method parts) "ssh"))
      (let ((host (file-remote-p default-directory 'host)))
        (dape--message "Remote connection detected, setting host to %S" host)
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
               (or (when-let* ((parent (dape--parent conn)))
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
  "Kill all dape buffers.
On SKIP-PROCESS-BUFFERS skip deletion of buffers which has processes."
  (cl-loop for buffer in (buffer-list)
           when (and (not (and skip-process-buffers
                               (get-buffer-process buffer)))
                     (when-let* ((name (buffer-name buffer)))
                       (string-match-p "\\*dape-.+\\*\\(<[0-9]+>\\)?$" name)))
           do (condition-case err
                  (let ((window (get-buffer-window buffer)))
                    (kill-buffer buffer)
                    (when (window-live-p window)
                      (delete-window window)))
                (error (message (error-message-string err))))))

(defun dape--display-buffer (buffer)
  "Display BUFFER according to `dape-buffer-window-arrangement'."
  (pcase-let*
      ((mode (buffer-local-value 'major-mode buffer))
       (group (cl-position (with-current-buffer buffer
                             (dape--info-window-group))
                           dape-info-buffer-window-groups))
       (`(,fns . ,alist)
        (pcase dape-buffer-window-arrangement
          ((or 'left 'right)
           (cons '(display-buffer-in-side-window)
                 (pcase (cons mode group)
                   (`(dape-repl-mode . ,_) '((side . bottom) (slot . -1)))
                   (`(dape-shell-mode . ,_) '((side . bottom) (slot . 0)))
                   (`(,_ . ,index) `((side . ,dape-buffer-window-arrangement)
                                     (slot . ,(1- index)))))))
          ('gud
           (pcase mode
             ('dape-repl-mode
              '((display-buffer-in-side-window) (side . top) (slot . -1)))
             ('dape-shell-mode
              '((display-buffer-pop-up-window)
                (direction . right) (dedicated . t)))
             ((guard group)
              `((display-buffer-in-side-window)
                ,@(nth group '(((side . top) (slot . 1))
                               ((side . bottom) (slot . -1))
                               ((side . bottom) (slot . 0))
                               ((side . top) (slot . 0))
                               ((side . bottom) (slot . 1))))))))
          (_ nil)))
       (category
        (when group (intern (format "dape-info-%s" group)))))
    (display-buffer buffer
                    `((display-buffer-reuse-window . ,fns)
                      (category . ,category) ,@alist))))

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

(defmacro dape--buffer-map (name fn &rest bindings)
  "Helper macro to create a keymap named NAME for an info buffer.
FN is bound to RET, mouse 2 and BINDINGS is a plist of (KEY FN) pairs
which is bound on map."
  (declare (indent defun))
  `(defvar ,name
     (let ((map (make-sparse-keymap)))
       (define-key map "\r" #',fn)
       (define-key map [mouse-2] #',fn)
       (define-key map [follow-link] 'mouse-face)
       ,@(cl-loop for (key f) on bindings by 'cddr
                  collect `(define-key map ,key ,f))
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
       (if (or ,@properties)
           (progn
             ,@body)
         (user-error "Command `%s' not available at point" ',name)))))

(defun dape--emacs-grab-focus ()
  "If `display-graphic-p' focus Emacs."
  (select-frame-set-input-focus (selected-frame)))

(define-minor-mode dape-many-windows
  "Display complex window layout if t and simple if nil.
The mode modifies `dape-start-hook' to remove or add the complex
layout for future debugging sessions."
  :global t
  :init-value t
  ;; Add or remove info buffers from `buffer-list' and hooks, based on
  ;; `dape-many-windows'.
  (if dape-many-windows
      (progn (add-hook 'dape-start-hook #'dape-info)
             (dape-info nil))
    (remove-hook 'dape-start-hook #'dape-info)
    (cl-loop for buffer in (dape--info-buffer-list)
             for window = (get-buffer-window buffer)
             when window do (quit-window t window)))
  ;; And make sure that Shell and Repl is displayed
  (when-let* ((buffer (get-buffer "*dape-shell*")))
    (dape--display-buffer buffer))
  (when-let* ((buffer (get-buffer "*dape-repl*"))
              (window (get-buffer-window buffer)))
    (quit-window nil window))
  (dape-repl))


;;; Connection

(defun dape--live-connection (type &optional nowarn require-selected)
  "Return connection instance of TYPE.
TYPE is expected to be one of the following symbols:
- parent: Parent connection.
- last: Last created child connection or parent which has an active
  thread.
- running: Last created child connection or parent which has an active
  thread but no stopped threads.
- stopped: Last created child connection or parent which has stopped
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
            ('parent (car connections))
            ('last (cl-find-if #'dape--thread-id ordered))
            ('running (cl-find-if (lambda (conn)
                                    (and (dape--thread-id conn)
                                         (not (dape--stopped-threads conn))))
                                  ordered))
            ('stopped (cl-find-if (lambda (conn)
                                    (and (dape--stopped-threads conn)))
                                  ordered)))))
    (unless (or nowarn conn)
      (user-error "No %sdebug connection"
                  ;; `parent' and `last' does not make sense to the user
                  (if (memq type '(running stopped))
                      (format "%s " type) "")))
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

(cl-defstruct (dape--breakpoint (:constructor dape--breakpoint-make))
  "Breakpoint object storing location and state."
  overlay path-line type value disabled hits verified id)

(cl-defmethod jsonrpc-convert-to-endpoint ((conn dape-connection)
                                           message subtype)
  "Convert jsonrpc CONN MESSAGE with SUBTYPE to DAP format."
  (cl-destructuring-bind (&key method id error params
                               (result nil result-supplied-p))
      message
    (with-slots (last-id n-sent-notifs) conn
      (cond ((eq subtype 'notification)
             `( :type "event"
                :seq ,(+ last-id (cl-incf n-sent-notifs))
                :event ,method
                :body ,params))
            ((eq subtype 'request)
             `( :type "request"
                :seq ,(+ (setq last-id id) n-sent-notifs)
                :command ,method
                ,@(when params `(:arguments ,params))))
            (error
             `( :type "response"
                :seq ,(+ (setq last-id id) n-sent-notifs)
                :request_seq ,last-id
                :success :json-false
                :message ,(plist-get error :message)
                :body ,(plist-get error :data)))
            (t
             `( :type "response"
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
           ;; Skipping :error field to skip error handling by signal
           `(:id ,request_seq :result ,dap-message))
          (command
           `(:id ,seq :method ,command :params ,arguments)))))


;;; Outgoing requests

(defconst dape--timeout-error "Request timeout"
  "Error string for request timeout.
Useful for `eq' comparison to derive request timeout error.")

(defvar dape--request-blocking nil
  "If non nil do request blocking.")

(defun dape-request (conn command arguments &optional cb)
  "Send request with COMMAND and ARGUMENTS to adapter CONN.
If callback function CB is supplied, it's called on timeout
and success.

CB will be called with PLIST and ERROR.
On success, ERROR will be nil.
On failure, ERROR will be an string.

If `dape--request-blocking' is non nil do blocking request."
  (cl-flet ((success-fn (result)
              (funcall cb (plist-get result :body)
                       (unless (eq (plist-get result :success) t)
                         (or (plist-get result :message) ""))))
            (timeout-fn ()
              (dape--warn
               "Command %S timed out after %d seconds (see \
`dape-request-timeout')"
               command
               dape-request-timeout)
              (funcall cb nil dape--timeout-error)))
    (if dape--request-blocking
        (let ((result (jsonrpc-request conn command arguments)))
          (when cb (success-fn result)))
      (jsonrpc-async-request conn command arguments
                             :success-fn
                             (when cb #'success-fn)
                             :error-fn #'ignore ;; will never be called
                             :timeout-fn
                             (when cb #'timeout-fn)
                             :timeout dape-request-timeout))))

(defun dape--initialize (conn)
  "Initialize CONN."
  (dape--with-request-bind
      (body error)
      (dape-request conn :initialize
                    `( :clientID "dape"
                       :adapterID ,(plist-get (dape--config conn) :type)
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
                       :supportsArgsCanBeInterpretedByShell t
                       :supportsProgressReporting t
                       :supportsStartDebuggingRequest t
                       ))
    (if error
        (progn
          (dape--warn "Initialize failed with %S" error)
          (dape-kill conn))
      (setf (dape--capabilities conn) body)
      ;; See `defer-launch-attach' in `dape-configs'
      (unless (plist-get (dape--config conn) 'defer-launch-attach)
        (dape--launch-or-attach conn)))))

(defun dape--launch-or-attach-arguments (conn)
  "Return plist of launch/attach arguments for CONN."
  ;; Transform config to jsonrpc serializable format
  ;; Remove all non `keywordp' keys and transform null to
  ;; :json-false
  (cl-labels
      ((transform-value (value)
         (pcase value
           ('nil :json-false)
           ;; Need a way to create json null values (see #72)
           (:null nil)
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

(defun dape--launch-or-attach (conn)
  "Launch or attach CONN."
  (dape--with-request-bind
      (_body error)
      (dape-request conn
                    (or (plist-get (dape--config conn) :request) :launch)
                    (dape--launch-or-attach-arguments conn))
    (when error
      (dape--warn "%s" error)
      (dape-kill conn))))

(defun dape--set-breakpoints-in-source (conn source &optional cb)
  "Set breakpoints in SOURCE for adapter CONN.
SOURCE is expected to be buffer or name of file.
See `dape-request' for expected CB signature."
  (cl-loop with breakpoints =
           (alist-get source (seq-group-by #'dape--breakpoint-source
                                           dape--breakpoints))
           for breakpoint in breakpoints
           for line = (dape--breakpoint-line breakpoint)
           unless (dape--breakpoint-disabled breakpoint)
           collect breakpoint into request-breakpoints and
           collect line into lines and
           collect
           (let ((source-breakpoint `(:line ,line)))
             (pcase (dape--breakpoint-type breakpoint)
               ('log
                (if (dape--capable-p conn :supportsLogPoints)
                    (plist-put source-breakpoint
                               :logMessage (dape--breakpoint-value breakpoint))
                  (dape--warn "Adapter does not support `dape-breakpoint-log'")))
               ('expression
                (if (dape--capable-p conn :supportsConditionalBreakpoints)
                    (plist-put source-breakpoint
                               :condition (dape--breakpoint-value breakpoint))
                  (dape--warn "Adapter does not support `dape-breakpoint-expression'")))
               ('hits
                (if (dape--capable-p conn :supportsHitConditionalBreakpoints)
                    (plist-put source-breakpoint
                               :hitCondition (dape--breakpoint-value breakpoint))
                  (dape--warn "Adapter does not support `dape-breakpoint-hits'"))))
             source-breakpoint)
           into source-breakpoints finally do
           (dape--with-request-bind
               ((&key ((:breakpoints updates)) &allow-other-keys) error)
               (dape-request
                conn :setBreakpoints
                (list
                 :source
                 (pcase source
                   ((pred stringp)
                    (list :path (dape--path-remote conn source)))
                   ((pred bufferp)
                    (or (cl-loop for (reference source-buffer) on dape--source-buffers by #'cddr
                                 when (eq source-buffer source) return
                                 (list :sourceReference reference))
                        (list :path (dape--path-remote conn (buffer-file-name source)))))
                   (_ (error "Should never be anything accept string or buffer")))
                 :breakpoints (apply #'vector source-breakpoints)
                 :lines (apply #'vector lines)))
             (if error
                 (dape--warn "Failed to set breakpoints in %s; %s" source error)
               (cl-loop for update across updates
                        for breakpoint in request-breakpoints do
                        (dape--breakpoint-update conn breakpoint update))
               (dape--request-continue cb error)))))

(defun dape--set-exception-breakpoints (conn &optional cb)
  "Set the exception breakpoints for adapter CONN.
The exceptions are derived from `dape--exceptions'.
See `dape-request' for expected CB signature."
  (if (not dape--exceptions)
      (dape--request-continue cb)
    (dape-request
     conn :setExceptionBreakpoints
     `(:filters
       ,(cl-map 'vector
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
    (dape--request-continue cb)))

(defun dape--set-breakpoints (conn cb)
  "Set breakpoints for adapter CONN.
See `dape-request' for expected CB signature."
  (if-let* ((sources
             (thread-last dape--breakpoints
                          (seq-group-by #'dape--breakpoint-source)
                          (mapcar #'car))))
      (cl-loop with responses = 0
               for source in sources do
               (dape--with-request (dape--set-breakpoints-in-source conn source)
                 (setf responses (1+ responses))
                 (when (length= sources responses)
                   (dape--request-continue cb))))
    (dape--request-continue cb)))

(defun dape--set-data-breakpoints (conn cb)
  "Set data breakpoints for adapter CONN.
See `dape-request' for expected CB signature."
  (if (dape--capable-p conn :supportsDataBreakpoints)
      (dape--with-request-bind
          ((&key breakpoints &allow-other-keys) error)
          (dape-request conn :setDataBreakpoints
                        (list
                         :breakpoints
                         (cl-loop
                          for plist in dape--data-breakpoints
                          collect (list :dataId (plist-get plist :dataId)
                                        :accessType (plist-get plist :accessType))
                          into breakpoints
                          finally return (apply #'vector breakpoints))))
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
           (dape--warn "Failed setting data breakpoints for %s"
                       (mapconcat (lambda (plist) (plist-get plist :name))
                                  unverfied-breakpoints ", ")))
         ;; FIXME Should not remove unverified-breakpoints as they
         ;; might be verified by another live connection.
         (setq dape--data-breakpoints verfied-breakpoints))
        (dape--request-continue cb error))
    (setq dape--data-breakpoints nil)
    (dape--request-continue cb)))

(defun dape--update-threads (conn cb)
  "Update threads for CONN in-place if possible.
See `dape-request' for expected CB signature."
  (dape--with-request-bind ((&key threads &allow-other-keys) error)
      (dape-request conn :threads nil)
    (setf (dape--threads conn)
          (mapcar
           (lambda (new-thread)
             (if-let* ((old-thread
                        (cl-find-if (lambda (old-thread)
                                      (eql (plist-get new-thread :id)
                                           (plist-get old-thread :id)))
                                    (dape--threads conn))))
                 (plist-put old-thread :name (plist-get new-thread :name))
               new-thread))
           (append threads nil)))
    (dape--maybe-select-thread conn
                               (cl-some (lambda (thread) (plist-get thread :id))
                                        (dape--threads conn)))
    (dape--request-continue cb error)))

(defun dape--stack-trace (conn thread nof cb)
  "Update stack trace in THREAD plist with NOF frames by adapter CONN.
See `dape-request' for expected CB signature."
  (let ((current-nof (length (plist-get thread :stackFrames)))
        (total-frames (plist-get thread :totalFrames))
        (value-formatting-p
         (dape--capable-p conn :supportsValueFormattingOptions))
        (delayed-stack-trace-p
         (dape--capable-p conn :supportsDelayedStackTraceLoading)))
    (if (or (not (equal (plist-get thread :status) 'stopped))
            (not (integerp (plist-get thread :id)))
            (eql current-nof total-frames)
            (and delayed-stack-trace-p (<= nof current-nof))
            (and (not delayed-stack-trace-p) (> current-nof 0)))
        (dape--request-continue cb)
      (dape--with-request-bind
          ((&key stackFrames totalFrames &allow-other-keys) error)
          (dape-request
           conn :stackTrace
           `( :threadId ,(plist-get thread :id)
              ,@(when delayed-stack-trace-p
                  `( :startFrame ,current-nof
                     :levels ,(- nof current-nof)))
              ,@(when (and dape-info-stack-buffer-modules value-formatting-p)
                  `(:format (:module t)))))
        (cond ((not delayed-stack-trace-p)
               (plist-put thread :stackFrames (append stackFrames nil)))
              ;; Sanity check delayed stack trace
              ((length= (plist-get thread :stackFrames) current-nof)
               (plist-put thread :stackFrames
                          (append (plist-get thread :stackFrames) stackFrames
                                  nil))))
        (plist-put thread :totalFrames (and (numberp totalFrames) totalFrames))
        (dape--request-continue cb error)))))

(defun dape--variables (conn object cb)
  "Update OBJECTs variables by adapter CONN.
See `dape-request' for expected CB signature."
  (let ((variables-reference (plist-get object :variablesReference)))
    (if (or (not (numberp variables-reference))
            (zerop variables-reference)
            (plist-get object :variables)
            (not (jsonrpc-running-p conn)))
        (dape--request-continue cb)
      (dape--with-request-bind
          ((&key variables &allow-other-keys) _error)
          (dape-request conn :variables
                        (list :variablesReference variables-reference))
        (plist-put object
                   :variables
                   (thread-last variables
                                (cl-map 'list #'identity)
                                (seq-filter #'identity)))
        (dape--request-continue cb)))))

(defun dape--variables-recursive (conn object path pred cb)
  "Update variables recursivly.
Get variable data from CONN and put result on OBJECT until PRED is nil.
PRED is called with PATH and OBJECT.
See `dape-request' for expected CB signature."
  (if-let* ((objects
             (cl-loop
              for variable in (or (plist-get object :scopes)
                                  (plist-get object :variables))
              for name = (plist-get variable :name)
              for expensive-p = (eq (plist-get variable :expensive) t)
              when (and (not expensive-p) (funcall pred (cons name path)))
              collect variable)))
      (let ((responses 0))
        (dolist (object objects)
          (dape--with-request (dape--variables conn object)
            (dape--with-request
                (dape--variables-recursive
                 conn object (cons (plist-get object :name) path) pred)
              (when (length= objects (cl-incf responses))
                (dape--request-continue cb))))))
    (dape--request-continue cb)))

(defun dape--evaluate-expression (conn frame-id expression context cb)
  "Send evaluate request to adapter CONN.
FRAME-ID specifies which frame the EXPRESSION is evaluated in and
CONTEXT which the result is going to be displayed in.
See `dape-request' for expected CB signature."
  (dape-request conn :evaluate
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
        (dape-request
         conn :setVariable
         (list :variablesReference variable-reference
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
        (dape-request
         conn :setExpression
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
  (if-let* ((id (plist-get stack-frame :id))
            ((not (plist-get stack-frame :scopes))))
      (dape--with-request-bind
          ((&key scopes &allow-other-keys) error)
          (dape-request conn :scopes (list :frameId id))
        (plist-put stack-frame :scopes (append scopes nil))
        (dape--request-continue cb error))
    (dape--request-continue cb)))

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
      (when display (dape--stack-frame-display conn))
      (dape--with-request
          (dape--scopes conn (dape--current-stack-frame conn))
        (run-hooks 'dape-update-ui-hook)))))


;;; Incoming requests

(cl-defgeneric dape-handle-request (_conn _command _arguments)
  "Sink for all unsupported requests." nil)

(define-derived-mode dape-shell-mode shell-mode "Shell"
  "Major mode for interacting with an debugged program."
  (setq-local revert-buffer-function (lambda (&rest _) (dape-restart))))

(cl-defmethod dape-handle-request (conn (_command (eql runInTerminal)) arguments)
  "Handle runInTerminal requests.
Starts a new adapter CONNs from ARGUMENTS."
  (let ((default-directory
         (or (when-let* ((cwd (plist-get arguments :cwd)))
               (dape--path-local conn cwd))
             default-directory))
        (process-environment
         (or (cl-loop for (key value) on (plist-get arguments :env) by 'cddr
                      collect (format "%s=%s" (substring (format "%s" key) 1) value))
             process-environment))
        (buffer (get-buffer-create "*dape-shell*")))
    (with-current-buffer buffer
      (dape-shell-mode)
      (shell-command-save-pos-or-erase))
    (let ((process
           (make-process :name "dape shell"
                         :buffer buffer
                         :command
                         (let ((args (append (plist-get arguments :args) nil)))
                           (if (plist-get arguments :argsCanBeInterpretedByShell)
                               (list shell-file-name shell-command-switch
                                     (mapconcat #'identity args " "))
                             args))
                         :filter
                         (if dape-repl-echo-shell-output
                             (lambda (process string)
                               (let ((before (marker-position (process-mark process))))
                                 (comint-output-filter process string)
                                 (dape--repl-insert
                                  (with-current-buffer (process-buffer process)
                                    (buffer-substring before (process-mark process))))))
                           #'comint-output-filter)
                         :sentinel #'shell-command-sentinel
                         :file-handler t)))
      (unless dape-repl-echo-shell-output (dape--display-buffer buffer))
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
  (setf (dape--initialized-p conn) t)
  (dape--update-state conn 'initialized)
  (dape--with-request (dape--configure-exceptions conn)
    (dape--with-request (dape--set-breakpoints conn)
      (dape--with-request (dape--set-data-breakpoints conn)
        (dape--with-request (dape-request conn :configurationDone nil)
          ;; See `defer-launch-attach' in `dape-configs'
          (when (plist-get (dape--config conn) 'defer-launch-attach)
            (dape--launch-or-attach conn)))))))

(cl-defmethod dape-handle-event (conn (_event (eql capabilities)) body)
  "Handle adapter CONNs capabilities events.
BODY is an plist of adapter capabilities."
  (setf (dape--capabilities conn)
        ;; Only changed capabilities needs to be included in body
        (append (plist-get body :capabilities) (dape--capabilities conn)))
  (dape--configure-exceptions conn))

(cl-defmethod dape-handle-event (conn (_event (eql breakpoint)) body)
  "Handle adapter CONNs breakpoint events.
Update `dape--breakpoints' according to BODY."
  (when-let* ((update (plist-get body :breakpoint))
              (id (plist-get update :id)))
    ;; Until `:reason' gets properly speced, try to infer update
    ;; intention, would prefer `pcase' on `:reason'.
    (if-let* ((breakpoint
               (cl-find id dape--breakpoints
                        :key (lambda (breakpoint)
                               (plist-get (dape--breakpoint-id breakpoint) conn)))))
        (dape--breakpoint-update conn breakpoint update)
      (unless (equal (plist-get body :reason) "removed")
        (dape--with-request (dape--source-ensure conn update)
          (when-let* ((marker (dape--object-to-marker conn update)))
            (dape--with-line (marker-buffer marker) (plist-get update :line)
              (dape--message "Creating breakpoint in %s:%d"
                             (buffer-name) (plist-get update :line))
              (dape--breakpoint-place))))))))

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
         (format "%sed" (or (plist-get body :startMethod) "start"))))
    (dape--update-state conn (intern start-method))
    (dape--message "%s %s" (capitalize start-method) (plist-get body :name))))

(cl-defmethod dape-handle-event (conn (_event (eql thread)) body)
  "Handle adapter CONNs thread events.
Stores `dape--thread-id' and updates/adds thread in
`dape--thread' from BODY."
  (cl-destructuring-bind (&key threadId reason &allow-other-keys)
      body
    (dape--maybe-select-thread conn threadId)
    (when (equal reason "started")
      ;; For adapters that does not send an continued request use
      ;; thread started as an way to switch from `initialized' to
      ;; running.
      (dape--update-state conn 'running))
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
              (seq-filter #'stringp
                          (list (plist-get body :text)
                                (plist-get body :description))))
             (str (concat (mapconcat #'identity texts ":\n\t") "\n")))
        (setf (dape--exception-description conn) str)
        (dape--repl-insert-error str)))
    ;; Update breakpoints hits
    (cl-loop for id across hitBreakpointIds
             for breakpoint =
             (cl-find id dape--breakpoints
                      :key (lambda (breakpoint)
                             (plist-get (dape--breakpoint-id breakpoint) conn)))
             when breakpoint do
             (with-slots (hits) breakpoint
               (setf hits (1+ (or hits 0)))))
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
    (dape--maybe-select-thread conn threadId)
    (dape--threads-set-status conn threadId (eq allThreadsContinued t) 'running
                              (dape--threads-make-update-handle conn))
    (run-hooks 'dape-update-ui-hook)))

(cl-defmethod dape-handle-event (_conn (_event (eql output)) body)
  "Handle output events by printing BODY with `dape--repl-message'."
  (when-let* ((output (plist-get body :output)))
    (pcase (plist-get body :category)
      ((or "stdout" "console" "output") (dape--repl-insert output))
      ("stderr" (dape--repl-insert-error output)))))

(cl-defmethod dape-handle-event (conn (_event (eql exited)) body)
  "Handle adapter CONNs exited events.
Prints exit code from BODY."
  (dape--update-state conn 'exited)
  (dape--stack-frame-cleanup)
  (dape--message "Exit code %d" (plist-get body :exitCode)))

(cl-defmethod dape-handle-event (conn (_event (eql terminated)) _body)
  "Handle adapter CONNs terminated events.
Killing the adapter and it's CONN."
  (let ((child-conn-p (dape--parent conn)))
    (dape--with-request (dape-kill conn)
      (when (not child-conn-p)
        ;; HACK remove duplicated terminated print for dlv
        (unless (eq (dape--state conn) 'terminated)
          (dape--message "Session terminated"))
        (dape--update-state conn 'terminated)
        (run-hooks 'dape-update-ui-hook)))))


;;; Startup/Setup

(defun dape--start-debugging (conn)
  "Preform some cleanup and start debugging with CONN."
  (unless (dape--parent conn)
    (dape--stack-frame-cleanup)
    (dape--breakpoints-reset)
    (cl-loop for (_ buffer) on dape--source-buffers by 'cddr
             when (buffer-live-p buffer)
             do (kill-buffer buffer))
    (setq dape--source-buffers nil)
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
        (process-environment (cl-copy-list process-environment))
        (command (cons (plist-get config 'command)
                       (cl-map 'list 'identity
                               (plist-get config 'command-args))))
        process server-process)
    ;; Initialize `process-environment' from `command-env'
    (cl-loop for (key value) on (plist-get config 'command-env) by 'cddr do
             (setenv (pcase key
                       ((pred keywordp) (substring (format "%s" key) 1))
                       ((or (pred symbolp) (pred stringp)) (format "%s" key))
                       (_ (user-error "Bad type for `command-env' key %S" key)))
                     (format "%s" value)))
    (cond
     (;; Socket connection
      (plist-get config 'port)
      ;; 1. Start server
      (when (plist-get config 'command)
        (let ((stderr-buffer
               (with-current-buffer (get-buffer-create " *dape-adapter stderr*")
                 (when (plist-get config 'command-insert-stderr)
                   (add-hook 'after-change-functions
                             (lambda (beg end _pre-change-len)
                               (dape--repl-insert-error (buffer-substring beg end)))
                             nil t))
                 (current-buffer))))
          (setq server-process
                (make-process :name "dape adapter"
                              :command command
                              :filter (lambda (_process string)
                                        (dape--repl-insert string))
                              :file-handler t
                              :buffer nil
                              :stderr stderr-buffer))
          (process-put server-process 'stderr-pipe stderr-buffer)
          ;; XXX Tramp does not allow `make-pipe-process' as :stderr,
          ;; `make-process' creates one for us with an unwanted
          ;; sentinel (`internal-default-process-sentinel').
          (when-let* ((pipe-process (get-buffer-process stderr-buffer)))
            (set-process-sentinel pipe-process #'ignore))
          (when dape-debug
            (dape--message "Adapter server started with %S"
                           (mapconcat #'identity command " "))))
        ;; FIXME Why do I need this?
        (when (file-remote-p default-directory)
          (sleep-for 0.300)))
      ;; 2. Connect to server
      (let ((host (or (plist-get config 'host) "localhost"))
            (retries 30))
        (while (and (not process) (> retries 0))
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
              (dape--warn "Unable to connect to dap server at %s:%d"
                          host (plist-get config 'port))
              (dape--message "Connection is configurable by `host' and `port' keys")
              ;; Barf server stderr
              (when-let* (server-process
                          (buffer (process-get server-process 'stderr-pipe))
                          (content (with-current-buffer buffer (buffer-string)))
                          ((not (string-empty-p content))))
                (dape--repl-insert-error (concat content "\n")))
              (delete-process server-process)
              (user-error "Unable to connect to server"))
          (when dape-debug
            (dape--message "%s to adapter established at %s:%s"
                           (if parent "Child connection" "Connection")
                           host (plist-get config 'port))))))
     (;; Pipe connection
      t
      (let ((command
             (cons (plist-get config 'command)
                   (cl-map 'list 'identity
                           (plist-get config 'command-args)))))
        (setq process
              (make-process :name "dape adapter"
                            :command command
                            :connection-type 'pipe
                            :coding 'utf-8-emacs-unix
                            :stderr (get-buffer-create "*dape-connection stderr*")
                            :file-handler t))
        (when dape-debug
          (dape--message "Adapter started with %S"
                         (mapconcat #'identity command " "))))))
    (dape-connection
     :name "dape-connection"
     :config config
     :parent parent
     :server-process server-process
     :events-buffer-config `(:size ,(if dape-debug nil 0) :format full)
     :on-shutdown
     (lambda (conn)
       (unless (dape--initialized-p conn)
         (dape--warn "Adapter %sconnection shutdown without successfully initializing"
                     (if (dape--parent conn) "child " "")))
       ;; Is this a complete shutdown?
       (unless (dape--parent conn)
         ;; Clean source buffer
         (dape--stack-frame-cleanup)
         ;; Kill server process
         (when-let* ((server-process (dape--server-process conn)))
           (delete-process server-process)
           (while (process-live-p server-process)
             (accept-process-output nil nil 0.1)))
         ;; Run hooks and update mode line
         (dape-active-mode -1)
         (force-mode-line-update t)))
     :request-dispatcher #'dape-handle-request
     :notification-dispatcher #'dape-handle-event
     :process process)))


;;; Commands

(defun dape-next (conn)
  "Step one line (skip functions)
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn :next))

(defun dape-step-in (conn)
  "Step into function/method.  If not possible behaves like `dape-next'.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn :stepIn))

(defun dape-step-out (conn)
  "Step out of function/method.  If not possible behaves like `dape-next'.
CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'stopped)))
  (dape--next-like-command conn :stepOut))

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
        (dape-request conn :continue body)
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
    (user-error "Thread is stopped"))
  (dape--with-request-bind
      (_body error)
      (dape-request conn :pause (dape--thread-id-object conn))
    (when error
      (error "Failed to pause: %s" error))))

(defun dape-restart (&optional conn skip-compile)
  "Restart debugging session.
CONN is inferred for interactive invocations.
SKIP-COMPILE is used internally for recursive calls."
  (interactive (list (dape--live-connection 'last t)))
  (dape--stack-frame-cleanup)
  (cond
   ;; Restart if adapter supports it
   ((and conn (dape--capable-p conn :supportsRestartRequest))
    (if (and (not skip-compile) (plist-get (dape--config conn) 'compile))
        (dape--compile (dape--config conn)
                       (lambda () (dape-restart conn 'skip-compile)))
      (dape--breakpoints-reset 'from-restart)
      (setq dape--connection-selected nil)
      (setf (dape--threads conn) nil
            (dape--thread-id conn) nil
            (dape--modules conn) nil
            (dape--sources conn) nil
            (dape--restart-in-progress-p conn) t)
      (dape-active-mode -1)
      (dape--with-request
          (dape-request conn :restart
                        `(:arguments ,(dape--launch-or-attach-arguments conn)))
        (setf (dape--restart-in-progress-p conn) nil))))
   ;; Use previous connections configuration
   (dape--connection (dape (dape--config dape--connection)))
   ;; Use history
   (dape-history
    (dape (apply #'dape--config-eval (dape--config-from-string (car dape-history)))))
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
        (dape-request conn :terminate nil)
      ;; We have to give up trying to kill the debuggee in an correct
      ;; way if the request timeout, otherwise we might force the
      ;; user to kill the process in some other way.
      (if (and error (not (eq error dape--timeout-error)))
          (dape-kill cb 'with-disconnect)
        (jsonrpc-shutdown conn)
        (dape--request-continue cb))))
   ((and conn (jsonrpc-running-p conn))
    (dape--with-request
        (dape-request conn :disconnect
                      `( :restart :json-false
                         ,@(when (dape--capable-p conn :supportTerminateDebuggee)
                             '(:terminateDebuggee t))))
      (jsonrpc-shutdown conn)
      (dape--request-continue cb)))
   (t
    (dape--request-continue cb))))

(defun dape-disconnect-quit (conn)
  "Kill adapter but try to keep debuggee live.
This will leave a decoupled debugged process with no debugge
connection.  CONN is inferred for interactive invocations."
  (interactive (list (dape--live-connection 'parent)))
  (dape--kill-buffers 'skip-process-buffers)
  (dape--with-request
      (dape-request conn :disconnect '(:terminateDebuggee :json-false))
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
  (if (cl-member nil (dape--breakpoints-at-point)
                 :key #'dape--breakpoint-type)
      (dape-breakpoint-remove-at-point)
    (dape--breakpoint-place)))

(defun dape-breakpoint-log (message)
  "Add log breakpoint at line with MESSAGE.
Expressions within `{}` are interpolated."
  (interactive
   (list
    (read-string "Log (Expressions within {} are interpolated): "
                 (when-let* ((breakpoint
                              (cl-find 'log (dape--breakpoints-at-point)
                                       :key #'dape--breakpoint-type)))
                   (dape--breakpoint-value breakpoint)))))
  (if (string-empty-p message)
      (dape-breakpoint-remove-at-point)
    (dape--breakpoint-place 'log message)))

(defun dape-breakpoint-expression (expression)
  "Add expression breakpoint at current line with EXPRESSION."
  ;; FIXME Rename to condition
  (interactive
   (list
    (read-string "Condition: "
                 (when-let* ((breakpoint
                              (cl-find 'expression (dape--breakpoints-at-point)
                                       :key #'dape--breakpoint-type)))
                   (dape--breakpoint-value breakpoint)))))
  (if (string-empty-p expression)
      (dape-breakpoint-remove-at-point)
    (dape--breakpoint-place 'expression expression)))

(defun dape-breakpoint-hits (condition)
  "Add hits breakpoint at line with CONDITION.
An hit HITS is an string matching regex:
\"\\(!=\\|==\\|[%<>]\\) [:digit:]\""
  (interactive
   (list
    (pcase-let ((`(_ ,operator)
                 (let (use-dialog-box)
                   (read-multiple-choice
                    "Operator" '((?= "==" "Equals") (?! "!=" "Not equals")
                                 (?< "<" "Less then") (?> ">" "Greater then")
                                 (?% "%" "Modulus"))))))
      (thread-last operator
                   (format "Breakpoint hit condition %s ")
                   (read-number)
                   (format "%s %d" operator)))))
  (if (string-empty-p condition)
      (dape-breakpoint-remove-at-point)
    (dape--breakpoint-place 'hits condition)))

(defun dape-breakpoint-remove-at-point (&optional skip-update)
  "Remove breakpoint, log breakpoint and expression at current line.
When SKIP-UPDATE is non nil, does not notify adapter about removal."
  (interactive)
  (cl-loop for breakpoint in (dape--breakpoints-at-point) do
           (dape--breakpoint-remove breakpoint skip-update)))

(defun dape-breakpoint-remove-all ()
  "Remove all breakpoints."
  (interactive)
  (cl-loop for (source . breakpoints) in
           (seq-group-by #'dape--breakpoint-source dape--breakpoints) do
           (cl-loop for breakpoint in breakpoints do
                    (dape--breakpoint-remove breakpoint 'skip-update))
           (dape--breakpoint-broadcast-update source)))

(defun dape-select-thread (conn thread-id)
  "Select current thread for adapter CONN by THREAD-ID.
With prefix argument thread is selected by index."
  (interactive
   (let* ((conn (dape--live-connection 'last))
          (collection
           (cl-loop with conns = (dape--live-connections)
                    with index = 0
                    for conn in conns append
                    (cl-loop for thread in (dape--threads conn) collect
                             (list (format "%s %s" (cl-incf index) (plist-get thread :name))
                                   conn
                                   (plist-get thread :id)))))
          (thread-name
           (if (numberp current-prefix-arg)
               (car (nth (1- current-prefix-arg) collection))
             (completing-read
              (format "Select thread (current %s): "
                      (thread-first conn (dape--current-thread)
                                    (plist-get :name)))
              collection nil t))))
     (alist-get thread-name collection nil nil #'equal)))
  (setf (dape--thread-id conn) thread-id)
  (setq dape--connection-selected conn)
  (dape--update conn nil t)
  (dape--mode-line-format))

(defun dape-select-stack (conn stack-id)
  "Selected current stack for adapter CONN by STACK-ID.
With prefix argument stack is selected by index."
  (interactive
   (let* ((conn (dape--live-connection 'stopped))
          (current-thread (dape--current-thread conn))
          (collection
           ;; Only one stack frame is guaranteed to be available,
           ;; so we need to reach out to make sure we got the full set.
           ;; See `dape--stack-trace'.
           (let ((dape--request-blocking t))
             (dape--with-request
                 (dape--stack-trace conn current-thread dape-stack-trace-levels))
             (mapcar (lambda (stack) (cons (plist-get stack :name)
                                           (plist-get stack :id)))
                     (plist-get current-thread :stackFrames))))
          (stack-name
           (if (numberp current-prefix-arg)
               (car (nth (1- current-prefix-arg) collection))
             (completing-read (format "Select stack (current %s): "
                                      (plist-get (dape--current-stack-frame conn) :name))
                              collection nil t))))
     (list conn (alist-get stack-name collection nil nil #'equal))))
  (setf (dape--stack-id conn) stack-id)
  (dape--update conn nil t))

(defun dape-stack-select-up (conn n)
  "Select N stacks above current selected stack for adapter CONN."
  (interactive (list (dape--live-connection 'stopped) 1))
  ;; Ensure all threads.  See `dape--stack-trace'.
  (let ((dape--request-blocking t))
    (dape--with-request
        (dape--stack-trace conn (dape--current-thread conn) dape-stack-trace-levels)))
  (if (dape--stopped-threads conn)
      (let* ((frames (plist-get (dape--current-thread conn) :stackFrames))
             (current-n (cl-position (dape--current-stack-frame conn) frames)))
        (dape-select-stack conn (plist-get (nth (+ current-n n) frames) :id)))
    (message "No stopped threads")))

(defun dape-stack-select-down (conn n)
  "Select N stacks below current selected stack for adapter CONN."
  (interactive (list (dape--live-connection 'stopped) 1))
  (dape-stack-select-up conn (* n -1)))

(defun dape-watch-dwim (expressions &optional no-add no-remove display)
  "Toggle a watch for EXPRESSIONS in the debugger.
If NO-ADD is non nil only allow removal of an existing watch.
If NO-REMOVE is non nil only allow adding a new watch.
If DISPLAY is non nil display the watch buffer."
  (interactive
   (list (mapcar
          #'string-trim
          (completing-read-multiple
           "Watch or unwatch symbol(s) or expression(s): "
           (mapcar (lambda (plist) (plist-get plist :name)) dape--watched)
           nil nil nil nil
           (or (and (region-active-p)
                    (buffer-substring (region-beginning) (region-end)))
               (thing-at-point 'symbol))))
         nil nil t))
  (dolist (expression expressions)
    (if-let* ((watched
               (cl-find expression dape--watched
                        :key (lambda (plist) (plist-get plist :name))
                        :test #'equal)))
        (unless no-remove
          (setq dape--watched (cl-delete watched dape--watched)))
      (unless no-add
        (push (list :name expression) dape--watched))))
  (when display
    (dape--display-buffer (dape--info-get-buffer-create 'dape-info-watch-mode)))
  (run-hooks 'dape-update-ui-hook))

(defun dape-evaluate-expression (conn expression)
  "Evaluate EXPRESSION, if region is active evaluate region.
EXPRESSION should be and string which can be evaluated in REPL.
CONN is inferred by either last stopped or last created connection."
  (interactive
   (list
    (or (dape--live-connection 'stopped t) (dape--live-connection 'last))
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Evaluate: " nil nil (thing-at-point 'symbol)))))
  (dape--with-request-bind
      ((&whole body &key variablesReference result &allow-other-keys) error)
      (dape--evaluate-expression conn (plist-get (dape--current-stack-frame conn) :id)
                                 expression "repl")
    (cond
     (error
      (if (string-empty-p error)
          (dape--warn "Failed to evaluate `%s'" (substring-no-properties expression))
        (dape--repl-insert-error (concat (string-trim-right error) "\n"))))
     ((and (get-buffer "*dape-repl*")
           (numberp variablesReference)
           (not (zerop variablesReference)))
      (dape--repl-insert
       (concat (dape--repl-variable (plist-put body :name expression)) "\n")))
     (t
      ;; Refresh is needed as evaluate can change values
      (dape--update conn 'variables nil)
      (dape--repl-insert (concat result "\n"))))))

(defun dape-restart-frame (conn stack-id)
  "Restart frame STACK-ID for adapter CONN."
  (interactive
   (let ((conn (dape--live-connection 'stopped t)))
     (list conn (dape--stack-id conn))))
  (unless (dape--capable-p conn :supportsRestartFrame)
    (user-error "Adapter not capable of restarting frame"))
  (dape-select-stack conn stack-id)
  (let* ((current-frame (dape--current-stack-frame conn))
	 (frame-id (plist-get current-frame :id)))
    (dape--with-request-bind (_body error)
	(dape-request conn :restartFrame `(:frameId ,frame-id))
      (when error
        (dape--warn "Failed to restart stack frame: %s" error)))))

;;;###autoload
(defun dape (config &optional skip-compile)
  "Start a debugging session using CONFIG.
When called interactively, the command prompts for a alist KEY from
`dape-configs', followed by additional property-value pairs.  These
pairs override the properties in the plist associated with the key
in `dape-configs'.

For example, interactively invoking:
  launch :program \"bin\"
executes the `launch' configuration from `dape-configs', overriding
the `:program' option with \"bin\".

CONFIG is an keyword-value plist, see VALUEs in `dape-config' alist.
SKIP-COMPILE argument is used internally for recursive calls
and should not be specified manually.

For more information see `dape-configs'."
  (interactive (list (dape--read-config)))
  (dape--with-request (dape-kill (dape--live-connection 'parent t))
    (dape--config-ensure config t)
    ;; Hooks need to be run before any REPL messaging but after we
    ;; have tried ensured that config is executable.
    (run-hooks 'dape-start-hook)
    (when-let* ((fn (or (plist-get config 'fn) 'identity))
                (fns (or (and (functionp fn) (list fn))
                         (and (listp fn) fn))))
      (setq config
            (seq-reduce (lambda (config fn) (funcall fn config))
                        (append fns dape-default-config-functions)
                        (copy-tree config))))
    (if (and (not skip-compile) (plist-get config 'compile))
        (dape--compile config (lambda () (dape config 'skip-compile)))
      (setq dape--connection (dape--create-connection config))
      (dape--start-debugging dape--connection))))


;;; Compile

(defvar-local dape--compile-after-fn nil)

(defun dape--compile-compilation-finish (buffer str)
  "Hook for `dape--compile-compilation-finish'.
Using BUFFER and STR."
  (remove-hook 'compilation-finish-functions #'dape--compile-compilation-finish)
  (if (equal "finished\n" str)
      (progn (funcall dape--compile-after-fn)
             (run-hook-with-args 'dape-compile-hook buffer))
    (dape--warn "Compilation failed \"%s\"" (string-trim-right str))))

(defun dape--compile (config fn)
  "Start compilation for CONFIG then call FN."
  (let ((default-directory (dape--guess-root config))
        (command (plist-get config 'compile)))
    (funcall dape-compile-function command)
    (with-current-buffer (compilation-find-buffer)
      (setq dape--compile-after-fn fn)
      (add-hook 'compilation-finish-functions #'dape--compile-compilation-finish nil t))))


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
        (dape-request conn :readMemory
                      (list :memoryReference dape--memory-address
                            :count dape-memory-page-size))
      (cond
       (error (message "Failed to read memory: %s" error))
       ((not data) (message "No bytes returned from adapter"))
       (t
        (setq dape--memory-address address
              hexl-max-address (1- dape-memory-page-size)
              buffer-undo-list nil)
        (let ((address (dape--memory-address-number))
              (temp-buffer (generate-new-buffer " *temp*" t))
              (buffer-empty-p (zerop (buffer-size))))
          (with-current-buffer temp-buffer
            (insert (base64-decode-string data))
            (let (buffer-undo-list)
              (hexlify-buffer))
            ;; Now we need to apply offset to the addresses, ughh
            (goto-char (point-min))
            (while (re-search-forward "^[0-9a-f]+" nil t)
              (let ((address
                     (thread-last (string-to-number (match-string 0) 16)
                                  (+ address)
                                  (format "%08x"))))
                (delete-region (match-beginning 0) (match-end 0))
                ;; `hexl' does not support address over 8 hex chars
                (insert (append (substring address (- (length address) 8)))))))
          (replace-region-contents (point-min) (point-max) temp-buffer)
          (when buffer-empty-p (hexl-goto-address 0))
          (kill-buffer temp-buffer))
        (set-buffer-modified-p nil)
        (when write-capable-p
          (add-hook 'write-contents-functions #'dape--memory-write)))))))

(defun dape--memory-write ()
  "Write buffer contents to stopped connection."
  (let ((conn (dape--live-connection 'last))
        (buffer (current-buffer))
        (address dape--memory-address))
    (with-temp-buffer
      (insert-buffer-substring buffer)
      (dehexlify-buffer)
      (dape--with-request-bind
          (_body error)
          (dape-request conn :writeMemory
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

(define-derived-mode dape-memory-mode hexl-mode "Memory"
  "Mode for reading and writing memory."
  :interactive nil
  (setq revert-buffer-function #'dape--memory-revert
        mode-line-buffer-identification
        (append mode-line-buffer-identification '(" {" dape--memory-address "}"))
        eldoc-documentation-functions nil))

(define-key dape-memory-mode-map "\C-x]" #'dape-memory-next-page)
(define-key dape-memory-mode-map "\C-x[" #'dape-memory-previous-page)

(defun dape-memory-next-page (&optional backward)
  "Move address `dape-memory-page-size' forward.
When BACKWARD is non nil move backward instead."
  (interactive nil dape-memory-mode)
  (dape-memory (format "0x%08x"
                       (funcall (if backward #'- #'+)
                                (dape--memory-address-number)
                                dape-memory-page-size))
               t))

(defun dape-memory-previous-page ()
  "Move address `dape-memory-page-size' backward."
  (interactive nil dape-memory-mode)
  (dape-memory-next-page 'backward))

(defun dape-memory-revert ()
  "Revert all `dape-memory-mode' buffers."
  (dape--with-debounce dape--memory-debounce-timer dape-ui-debounce-time
    (cl-loop for buffer in (buffer-list)
             when (eq (buffer-local-value 'major-mode buffer) 'dape-memory-mode)
             do (with-current-buffer buffer (revert-buffer)))))

(define-obsolete-variable-alias 'dape-read-memory 'dape-memory "0.24.0")
(defun dape-memory (address &optional reuse-buffer)
  "View and edit memory from ADDRESS in hex dump format.
If REUSE-BUFFER is non nil reuse the current buffer to display result
of memory read."
  (interactive
   (list (string-trim
          (read-string "View memory at address: " nil nil
                       (when-let* ((number (thing-at-point 'number)))
                         (format "0x%08x" number))))))
  (let ((conn (dape--live-connection 'stopped)))
    (unless (dape--capable-p conn :supportsReadMemoryRequest)
      (user-error "Adapter not capable of reading memory"))
    (let ((buffer (if reuse-buffer (current-buffer)
                    (generate-new-buffer "*dape-memory*"))))
      (with-current-buffer buffer
        (unless (eq major-mode 'dape-memory-mode)
          (dape-memory-mode)
          (when (dape--capable-p conn :supportsWriteMemoryRequest)
            (message (substitute-command-keys
                      "Write memory with `\\[save-buffer]'"))))
        (setq dape--memory-address address)
        (revert-buffer))
      (select-window (display-buffer buffer)))))


;;; Disassemble viewer

(defvar-local dape--disassemble-overlay-arrow nil)

(add-to-list 'overlay-arrow-variable-list 'dape--disassemble-overlay-arrow)

(define-derived-mode dape-disassemble-mode asm-mode "Disassemble"
  :interactive nil
  (setq-local dape--disassemble-overlay-arrow (make-marker)
              dape-stepping-granularity 'instruction))

(defvar dape--disassemble-debounce-timer (timer-create)
  "Debounce context for `dape-disassemble-revert'.")

(defun dape-disassemble-revert ()
  "Revert all `dape-disassemble-mode' buffers."
  (dape--with-debounce dape--disassemble-debounce-timer dape-ui-debounce-time
    (cl-loop for buffer in (buffer-list)
             when (eq (buffer-local-value 'major-mode buffer)
                      'dape-disassemble-mode)
             do (with-current-buffer buffer (revert-buffer)))))

(defun dape-disassemble (address)
  "View disassemble of instructions at ADDRESS."
  (interactive
   (list (string-trim
          (read-string
           "Disassemble at address: " nil nil
           `(,@(when-let* ((number (thing-at-point 'number)))
                 (list (format "0x%08x" number)))
             ,@(when-let* ((conn (dape--live-connection 'stopped t))
                           (address (plist-get (dape--current-stack-frame conn)
                                               :instructionPointerReference)))
                 (list address)))))))
  (if-let* ((conn (dape--live-connection 'stopped))
            ((not (dape--capable-p conn :supportsDisassembleRequest))))
      (user-error "Adapter does not support disassemble")
    (dape--with-request-bind
        ((&key ((:instructions instructions)) &allow-other-keys) _)
        (dape-request conn :disassemble
                      `( :memoryReference ,address
                         :instructionCount 100
                         :offset 0
                         :instructionOffset -50
                         :resolveSymbols t))
      (cl-flet ((address-to-int (address)
                  (string-to-number (substring address 2) 16)))
        (with-current-buffer (get-buffer-create "*dape-disassemble*")
          (dape-disassemble-mode)
          (erase-buffer)
          (cl-loop
           with last-symbol with last-location
           with ps =
           (address-to-int (plist-get (dape--current-stack-frame conn)
                                      :instructionPointerReference))
           with source = (plist-get (dape--current-stack-frame conn) :source)
           with line = (plist-get (dape--current-stack-frame conn) :line)
           for instruction across instructions
           for address = (address-to-int (plist-get instruction :address))
           for current-instruction-p = (equal address ps)
           for location =
           (setq last-location
                 ;; Forward fill all location if missing as per spec
                 (or (plist-get instruction :location) last-location))
           for current-line-p =
           (and (equal location source)
                (equal (plist-get instruction :line) line))
           do
           (when-let* ((symbol (plist-get instruction :symbol))
                       ((not (equal last-symbol symbol))))
             (insert
              (concat "; " (setq last-symbol symbol) " of "
                      (thread-first instruction
                                    (plist-get :location)
                                    (plist-get :name)))
              ":\n"))
           (when current-instruction-p
             (move-marker dape--disassemble-overlay-arrow (point)))
           (insert
            (propertize
             (format "%s:\t%s\n"
                     (plist-get instruction :address)
                     (plist-get instruction :instruction))
             'line-prefix
             (when current-line-p
               (dape--indicator "|" 'vertical-bar nil))
             'dape--disassemble-instruction instruction)))
          (setq-local revert-buffer-function
                      (lambda (&rest _) (dape-disassemble address)))
          (select-window (display-buffer (current-buffer)))
          (goto-char (or (marker-position dape--disassemble-overlay-arrow)
                         (point-min)))
          (run-hooks 'dape-display-source-hook))))))


;;; Breakpoints

(defun dape--breakpoint-buffer (breakpoint)
  "Return a buffer visiting BREAKPOINT if one exist."
  (with-slots (overlay) breakpoint
    (when overlay
      (overlay-buffer overlay))))

(defun dape--breakpoint-path (breakpoint)
  "Return path for BREAKPOINT if one exist."
  (with-slots (overlay path-line) breakpoint
    (if overlay
        (buffer-file-name (overlay-buffer overlay))
      (car path-line))))

(defun dape--breakpoint-line (breakpoint)
  "Return line for BREAKPOINT."
  (with-slots (overlay path-line) breakpoint
    (if overlay
        (with-current-buffer (overlay-buffer overlay)
          (line-number-at-pos (overlay-start overlay)))
      (cdr path-line))))

(defun dape--breakpoints-in-buffer ()
  "Return breakpoints in current buffer."
  (thread-last dape--breakpoints
               (seq-group-by #'dape--breakpoint-buffer)
               (alist-get (current-buffer))))

(defun dape--breakpoint-set-overlay (breakpoint)
  "Create and set overlay on BREAKPOINT."
  (add-hook 'kill-buffer-hook #'dape--breakpoint-buffer-kill nil t)
  (with-slots (type value overlay disabled) breakpoint
    (cl-flet ((after-string (ov label face mouse-1-help mouse-1-def)
                (overlay-put
                 ov 'after-string
                 (concat " "
                         (propertize
                          (format "%s: %s" label value)
                          'face face
                          'mouse-face 'highlight
                          'help-echo (format "mouse-1: %s" mouse-1-help)
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1] mouse-1-def)
                                    map))))))
      (let ((ov (apply #'make-overlay (dape--overlay-region)))
            (maybe-disabled-face (when disabled 'shadow)))
        (overlay-put ov 'modification-hooks '(dape--breakpoint-freeze))
        (overlay-put ov 'category 'dape-breakpoint)
        (overlay-put ov 'window t)
        (pcase type
          ('log
           (after-string ov "Log" (or maybe-disabled-face 'dape-log-face)
                         "edit log message" #'dape-mouse-breakpoint-log))
          ('expression
           (after-string ov "Cond" (or maybe-disabled-face 'dape-expression-face)
                         "edit break condition" #'dape-mouse-breakpoint-expression))
          ('hits
           (after-string ov "Hits" (or maybe-disabled-face 'dape-hits-face)
                         "edit break hit condition"
                         #'dape-mouse-breakpoint-hits))
          ('until
           (overlay-put ov 'before-string
                        (dape--indicator dape-breakpoint-margin-string 'breakpoint
                                         'dape-breakpoint-until-face)))
          (_
           (overlay-put ov 'before-string
                        (dape--indicator dape-breakpoint-margin-string 'breakpoint
                                         (or maybe-disabled-face 'dape-breakpoint-face)))))
        (setf overlay ov)))))

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
    (define-key map [left-fringe mouse-1] #'dape-mouse-breakpoint-toggle)
    (define-key map [left-margin mouse-1] #'dape-mouse-breakpoint-toggle)
    ;; TODO Would be nice if mouse-2 would open an menu for any
    ;; breakpoint type (expression, log and hit).
    (define-key map [left-fringe mouse-2] #'dape-mouse-breakpoint-expression)
    (define-key map [left-margin mouse-2] #'dape-mouse-breakpoint-expression)
    (define-key map [left-fringe mouse-3] #'dape-mouse-breakpoint-log)
    (define-key map [left-margin mouse-3] #'dape-mouse-breakpoint-log)
    map)
  "Keymap for `dape-breakpoint-global-mode'.")

(define-minor-mode dape-breakpoint-global-mode
  "Adds fringe and margin breakpoint controls."
  :global t
  :lighter nil)

(defun dape--breakpoint-maybe-remove-ff-hook ()
  "Remove `find-file-hook' if all breakpoints have buffers."
  (cl-loop for breakpoint in dape--breakpoints
           always (bufferp (dape--breakpoint-source breakpoint))
           finally do (remove-hook 'find-file-hook
                                   #'dape--breakpoint-find-file-hook)))

(defun dape--breakpoint-find-file-hook ()
  "Convert PATH-LINE breakpoints into overlay breakpoints.
Used as an hook on `find-file-hook'."
  (when (buffer-file-name (current-buffer))
    (cl-loop with breakpoints-in-buffer =
             (alist-get (buffer-file-name)
                        (seq-group-by #'dape--breakpoint-path dape--breakpoints)
                        nil nil #'equal)
             for breakpoint in breakpoints-in-buffer
             for line = (dape--breakpoint-line breakpoint)
             unless (dape--breakpoint-buffer breakpoint) do
             (dape--with-line (current-buffer) line
               (dape--breakpoint-set-overlay breakpoint))
             (run-hooks 'dape-update-ui-hook)))
  (dape--breakpoint-maybe-remove-ff-hook))

(defvar-local dape--original-margin nil
  "Bookkeeping for buffer margin width.")

(defun dape--indicator (string bitmap face)
  "Return indicator string in margin (STRING) or fringe (BITMAP).
The indicator is `propertize'd with with FACE."
  (if (and (window-system)
           (not (eql (frame-parameter (selected-frame) 'left-fringe) 0)))
      (propertize " " 'display `(left-fringe ,bitmap ,face))
    (setq-local dape--original-margin (or dape--original-margin
                                          left-margin-width)
                left-margin-width 2)
    (set-window-margins (selected-window) left-margin-width)
    (propertize " " 'display `((margin left-margin)
                               ,(propertize string 'face face)))))

(defun dape--breakpoint-freeze (overlay _after _begin _end &optional _len)
  "Make sure that OVERLAY region covers line."
  (apply #'move-overlay overlay (dape--overlay-region)))

(defun dape--breakpoints-reset (&optional from-restart)
  "Reset breakpoints hits.
If FROM-RESTART is non nil keep id and verified."
  (cl-loop for breakpoint in dape--breakpoints do
           (with-slots (verified id hits) breakpoint
             (unless from-restart
               (setf verified nil id nil))
             (setf hits nil))))

(defun dape--breakpoints-at-point ()
  "Return list of breakpoints at point."
  (cl-loop with current-line = (line-number-at-pos (point))
           for breakpoint in dape--breakpoints
           when (and (eq (current-buffer) (dape--breakpoint-buffer breakpoint))
                     (equal current-line (dape--breakpoint-line breakpoint)))
           collect breakpoint))

(defun dape--breakpoint-broadcast-update (&rest sources)
  "Broadcast breakpoints in SOURCES to all connections."
  (cl-loop with sources = (cl-remove-duplicates sources :test #'equal)
           for source in sources when source do
           (cl-loop for conn in (dape--live-connections)
                    when (dape--initialized-p conn) do
                    (dape--set-breakpoints-in-source conn source)))
  (run-hooks 'dape-update-ui-hook))

(defun dape--breakpoint-buffer-kill (&rest _)
  "Hook to remove breakpoint on buffer killed."
  (cl-loop for breakpoint in (dape--breakpoints-in-buffer)
           for line = (dape--breakpoint-line breakpoint)
           if (buffer-file-name (current-buffer)) do
           (with-slots (overlay) breakpoint
             (when overlay
               (add-hook 'find-file-hook #'dape--breakpoint-find-file-hook)
               (delete-overlay overlay))
             (setf overlay nil)
             (setf (dape--breakpoint-path-line breakpoint)
                   (cons (buffer-file-name (current-buffer)) line)))
           else do
           (dape--breakpoint-remove breakpoint)))

(defun dape--breakpoint-place (&optional type value)
  "Place breakpoint at current line.
Valid values for TYPE is nil, `log', `expression' and `hits'.
If TYPE is non nil VALUE is expected to be an string.
If there are breakpoints at current line remove those breakpoints from
`dape--breakpoints'.  Updates all breakpoints in all known connections."
  (unless (derived-mode-p 'prog-mode)
    (user-error "Trying to set breakpoint in none `prog-mode' buffer"))
  ;; Remove breakpoints at line
  (dape-breakpoint-remove-at-point 'skip-update)
  ;; Create breakpoint
  (let ((breakpoint (dape--breakpoint-make :type type :value value)))
    (dape--breakpoint-set-overlay breakpoint)
    (push breakpoint dape--breakpoints)
    ;; ...and push to adapter
    (dape--breakpoint-broadcast-update (current-buffer))
    breakpoint))

(defun dape--breakpoint-delete-overlay (breakpoint)
  "Delete of BREAKPOINT overlay.
Handling restoring margin if necessary."
  (let ((buffer (dape--breakpoint-buffer breakpoint)))
    (with-slots (overlay) breakpoint
      (when overlay (delete-overlay overlay))
      (setf overlay nil))
    (when (and
           ;; Buffer margin has been touched
           dape--original-margin
           ;; Buffer has no breakpoint in margin
           (not (cl-some (lambda (breakpoint)
                           (not (dape--breakpoint-type breakpoint)))
                         (dape--breakpoints-in-buffer))))
      ;; Reset margin
      (setq-local left-margin-width dape--original-margin
                  dape--original-margin nil)
      (set-window-margins (selected-window)
                          left-margin-width right-margin-width)
      (when-let* ((window (get-buffer-window buffer)))
        (set-window-buffer window buffer)))))

(defun dape--breakpoint-disable (breakpoint disabled)
  "Swap BREAKPOINT overlay to DISABLED state."
  (setf (dape--breakpoint-disabled breakpoint) disabled)
  (when-let* ((buffer (dape--breakpoint-source breakpoint))
              (line (dape--breakpoint-line breakpoint))
              ((bufferp buffer)))
    (dape--breakpoint-delete-overlay breakpoint)
    (dape--with-line buffer line (dape--breakpoint-set-overlay breakpoint))))

(defun dape--breakpoints-update ()
  "Broadcast breakpoint state in all breakpoint sources."
  (thread-last dape--breakpoints
               (seq-group-by #'dape--breakpoint-source)
               (mapcar #'car)
               (apply #'dape--breakpoint-broadcast-update)))

(defun dape--breakpoint-remove (breakpoint &optional skip-update)
  "Remove BREAKPOINT breakpoint from buffer and session.
When SKIP-UPDATE is non nil, does not notify adapter about removal."
  (setq dape--breakpoints (delq breakpoint dape--breakpoints))
  (unless skip-update
    (dape--breakpoint-broadcast-update (dape--breakpoint-source breakpoint)))
  (dape--breakpoint-delete-overlay breakpoint)
  (dape--breakpoint-maybe-remove-ff-hook)
  (run-hooks 'dape-update-ui-hook))

(defun dape--breakpoint-source (breakpoint)
  "Return the source of BREAKPOINT.
The source is either a buffer or a file path."
  (if-let* ((buffer (dape--breakpoint-buffer breakpoint)))
      buffer
    (dape--breakpoint-path breakpoint)))

(defun dape--breakpoint-update (conn breakpoint update)
  "Update BREAKPOINT with UPDATE plist from CONN."
  (with-slots (id verified type value disabled) breakpoint
    (unless disabled
      ;; Update `dape--breakpoint' data
      (setf id (plist-put id conn (plist-get update :id))
            verified (plist-put verified conn
                                (eq (plist-get update :verified) t)))
      ;; Move breakpoints and update state at adapters
      (let ((buffer (dape--breakpoint-buffer breakpoint))
            (line (dape--breakpoint-line breakpoint))
            (new-line (plist-get update :line)))
        ;; Guard for infinite breakpoint updates
        (when (and (numberp line) (numberp new-line)
                   (not (eq line new-line)))
          (dape--breakpoint-delete-overlay breakpoint)
          (if buffer
              (dape--with-line buffer new-line
                (dape-breakpoint-remove-at-point 'skip-update)
                (dape--breakpoint-set-overlay breakpoint)
                (pulse-momentary-highlight-region
                 (line-beginning-position) (line-beginning-position 2) 'next-error))
            (setcdr (dape--breakpoint-path-line breakpoint) new-line))
          ;; Update breakpoint state with all connections
          (dape--breakpoint-broadcast-update (dape--breakpoint-source breakpoint))
          (dape--message "Breakpoint in %s moved from line %s to %s"
                         (if buffer (buffer-name buffer)
                           (dape--breakpoint-path breakpoint))
                         line new-line)))))
  (run-hooks 'dape-update-ui-hook))

(defun dape-breakpoint-load (&optional file)
  "Load breakpoints from FILE.
All breakpoints will be removed before loading new ones.
Will open buffers containing breakpoints.
Will use `dape-default-breakpoints-file' if FILE is nil."
  (interactive
   (list (read-file-name "Load breakpoints from file: ")))
  (setq file (or file dape-default-breakpoints-file))
  (when (file-exists-p file)
    (dape-breakpoint-remove-all)
    (let ((breakpoints
           (with-temp-buffer
             (insert-file-contents file)
             (goto-char (point-min))
             (nreverse (read (current-buffer))))))
      (cl-loop for (file line type value) in breakpoints do
               (if (find-buffer-visiting file)
                   (dape--with-line (find-file-noselect file) line
                     (dape--breakpoint-place type value))
                 (add-hook 'find-file-hook #'dape--breakpoint-find-file-hook)
                 (push (dape--breakpoint-make :path-line (cons file line)
                                              :type type
                                              :value value)
                       dape--breakpoints))))
    (dape--breakpoints-update)))

(defun dape-breakpoint-save (&optional file)
  "Save breakpoints to FILE.
Will use `dape-default-breakpoints-file' if FILE is nil."
  (interactive
   (list
    (read-file-name "Save breakpoints to file: ")))
  (setq file (or file dape-default-breakpoints-file))
  (with-temp-buffer
    (insert
     ";; Generated by `dape-breakpoint-save'\n"
     ";; Load breakpoints with `dape-breakpoint-load'\n\n")
    (cl-loop for breakpoint in dape--breakpoints
             for path = (dape--breakpoint-path breakpoint)
             when path collect
             (list path
                   (dape--breakpoint-line breakpoint)
                   (dape--breakpoint-type breakpoint)
                   (dape--breakpoint-value breakpoint))
             into serialized finally do
             (prin1 serialized (current-buffer)))
    ;; Skip write if nothing has changed since last save
    (unless (and (file-exists-p file)
                 (equal (buffer-string)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
      (write-region (point-min) (point-max) file nil
                    (unless (called-interactively-p 'interactive) 'quiet)))))


;;; Source buffers

(defun dape--source-ensure (conn plist cb)
  "Ensure that source object in PLIST exist for adapter CONN.
See `dape-request' for expected CB signature."
  (let* ((source (plist-get plist :source))
         (path (plist-get source :path))
         (refrence (plist-get source :sourceReference))
         (buffer (plist-get dape--source-buffers refrence)))
    (cond
     ((or (and (stringp path) (file-exists-p (dape--path-local conn path)) path)
          (and (buffer-live-p buffer) buffer))
      (dape--request-continue cb))
     ((and (numberp refrence) (< 0 refrence) refrence)
      (dape--with-request-bind
          ((&key content mimeType &allow-other-keys) error)
          (dape-request conn :source
                        (list :source source :sourceReference refrence))
        (cond (error (dape--warn "%s" error))
              (content
               (let ((buffer
                      (generate-new-buffer
                       (format "*dape-source %s*" (plist-get source :name)))))
                 (setq dape--source-buffers
                       (plist-put dape--source-buffers
                                  (plist-get source :sourceReference) buffer))
                 (with-current-buffer buffer
                   (when mimeType
                     (if-let* ((mode
                                (alist-get mimeType dape-mime-mode-alist nil nil #'equal)))
                         (unless (eq major-mode mode)
                           (funcall mode))
                       (message "Unknown mime type %s, see `dape-mime-mode-alist'"
                                mimeType)))
                   (setq buffer-read-only t)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert content))
                   (goto-char (point-min)))
                 (dape--request-continue cb)))))))))


;;; Stack frame source

(defvar dape--overlay-arrow-position (make-marker)
  "Dape stack position marker.")

(add-to-list 'overlay-arrow-variable-list 'dape--overlay-arrow-position)

(defvar dape--stack-position-overlay nil
  "Dape stack position overlay for line.")

(defun dape--stack-frame-cleanup ()
  "Cleanup after `dape--stack-frame-display'."
  (when-let* ((buffer (marker-buffer dape--overlay-arrow-position)))
    (with-current-buffer buffer
      (dape--remove-eldoc-hook)))
  (when (overlayp dape--stack-position-overlay)
    (delete-overlay dape--stack-position-overlay))
  (set-marker dape--overlay-arrow-position nil))

(defun dape--stack-frame-display-1 (conn frame deepest-p)
  "Display FRAME for adapter CONN as if DEEPEST-p.
Helper for `dape--stack-frame-display'."
  (dape--with-request (dape--source-ensure conn frame)
    ;; Delete overlay before dropping the reference
    (dape--stack-frame-cleanup)
    (when-let* ((marker (dape--object-to-marker conn frame)))
      (with-current-buffer (marker-buffer marker)
        (dape--add-eldoc-hook)
        (save-excursion
          (goto-char (marker-position marker))
          (setq dape--stack-position-overlay
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-beginning-position 2))))
                  (overlay-put ov 'category 'dape-source-line)
                  (overlay-put ov 'face 'dape-source-line-face)
                  (when-let* (deepest-p
                              (description (dape--exception-description conn)))
                    (overlay-put ov 'after-string
                                 (propertize description 'face
                                             'dape-exception-description-face)))
                  ov)
                fringe-indicator-alist
                (unless deepest-p
                  '((overlay-arrow . hollow-right-triangle))))
          ;; Move `overaly-arrow' arrow to point
          (move-marker dape--overlay-arrow-position
                       (line-beginning-position)))
        (when-let* ((window
                     (display-buffer (marker-buffer marker)
                                     dape-display-source-buffer-action)))
          ;; Change selected window if not `dape-repl' buffer is selected
          (unless (with-current-buffer (window-buffer)
                    (cl-some #'derived-mode-p '(dape-repl-mode
                                                dape-info-parent-mode
                                                dape-disassemble-mode)))
            (select-window window))
          (with-selected-window window
            ;; XXX This is where point is moved after step commands.
            ;; Which means that `post-command-hook' has already run.
            ;; Can't call the hook directly from timer context but can
            ;; handle the important bits.
            (goto-char (marker-position marker))
            ;; ...like fixing `hl-line'
            (when (featurep 'hl-line)
	      (cond (global-hl-line-mode (global-hl-line-highlight))
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
                  (cl-loop
                   with frames = (plist-get thread :stackFrames)
                   for cell on frames for (frame . _rest) = cell
                   when (eq frame selected) return
                   (cl-loop
                    for frame in cell
                    for source = (plist-get frame :source) when
                    (or (when-let* ((reference (plist-get source :sourceReference)))
                          (< 0 reference))
                        (when-let* ((remote-path (plist-get source :path))
                                    (path (dape--path-local conn remote-path)))
                          (file-exists-p path)))
                    return frame))))
        ;; Check if `displayable-frame' PLIST exist, otherwise fetch all
        (if-let* ((frame (displayable-frame)))
            (dape--stack-frame-display-1 conn frame deepest-p)
          (dape--with-request (dape--stack-trace conn thread dape-stack-trace-levels)
            (when-let* ((frame (displayable-frame)))
              (dape--stack-frame-display-1 conn frame deepest-p))))))))


;;; Info Buffers

(defvar-local dape--info-buffer-related nil "List of related buffers.")
(defvar-local dape--info-buffer-index nil "Per mode buffer index.")

(defvar dape--info-buffers nil "List containing `dape-info' buffers.")

(defvar dape--info-buffer-display-history nil "History list in (MODE INDEX).")

(defun dape--info-buffer-list ()
  "Return all live `dape-info-parent-mode'."
  (setq dape--info-buffers
        (cl-delete-if-not #'buffer-live-p dape--info-buffers)))

(defun dape--info-buffer-p (mode &optional index)
  "Is buffer of MODE with INDEX."
  (and (derived-mode-p mode)
       (or (not index) (equal dape--info-buffer-index index))))

(defun dape--info-window-group ()
  "Return current buffer's info group.
See `dape-info-buffer-window-groups'."
  (cl-find-if (lambda (group)
                (cl-some (lambda (spec)
                           (apply #'dape--info-buffer-p (ensure-list spec)))
                         group))
              dape-info-buffer-window-groups))

(defun dape-info-buffer-tab (&optional reversed)
  "Select next related buffer in `dape-info' buffers.
REVERSED selects previous."
  (interactive)
  (unless dape--info-buffer-related
    (user-error "No related buffers for current buffer"))
  (pcase-let* ((order-fn (if reversed 'reverse 'identity))
               (`(,mode ,index)
                (or (thread-last dape--info-buffer-related
                                 (append  dape--info-buffer-related)
                                 (funcall order-fn)
                                 (seq-drop-while (pcase-lambda (`(,mode ,index))
                                                   (not (dape--info-buffer-p mode index))))
                                 (cadr))
                    (car dape--info-buffer-related))))
    (push `(,mode ,index) dape--info-buffer-display-history)
    (gdb-set-window-buffer (dape--info-get-buffer-create mode index) t)))

(defvar dape-info-parent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>")
                (lambda () (interactive) (dape-info-buffer-tab t)))
    (define-key map "\t" #'dape-info-buffer-tab)
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
  "Generic mode to derive all other info buffer modes from."
  :interactive nil
  (setq-local buffer-read-only t
              truncate-lines t
              cursor-in-non-selected-windows nil
              revert-buffer-function #'dape--info-revert
              dape--info-debounce-timer (timer-create))
  (add-hook 'window-buffer-change-functions #'dape--info-buffer-change-fn
            nil 'local)
  (when dape-info-hide-mode-line (setq-local mode-line-format nil))
  (buffer-disable-undo))

(defun dape--info-header (name mode index help-echo mouse-face face)
  "Helper to create buffer header.
Creates header with string NAME, mouse map to select buffer
identified with MODE and INDEX (see `dape--info-buffer-index')
with HELP-ECHO string, MOUSE-FACE and FACE."
  (let ((command
         (lambda (event)
           (interactive "e")
           (save-selected-window
             (select-window (posn-window (event-start event)))
             (let ((buffer
                    (dape--info-get-buffer-create mode index)))
               (with-current-buffer buffer (revert-buffer))
               (push `(,mode ,index) dape--info-buffer-display-history)
               (gdb-set-window-buffer buffer t)))))
        (map (make-sparse-keymap)))
    (define-key map (vector 'header-line 'mouse-1) command)
    (define-key map (vector 'header-line 'down-mouse-1) command)
    (propertize name 'help-echo help-echo 'mouse-face mouse-face 'face face
                'keymap map)))

(defun dape--info-call-update-with (fn)
  "Helper for `dape--info-revert' functions.
Erase BUFFER content and updates `header-line-format'.
FN is expected to update insert buffer contents, update
`dape--info-buffer-related' and `header-line-format'."
  ;; Save buffer as `select-window' sets buffer
  (save-current-buffer
    (when (derived-mode-p 'dape-info-parent-mode)
      ;; Would be nice with `replace-buffer-contents', but it messes
      ;; up string properties
      (let ((line (line-number-at-pos (point) t))
            (old-window (selected-window)))
        ;; Try to keep point and scroll
        (when-let* ((window (get-buffer-window)))
          (select-window window))
        (save-window-excursion
          (let ((inhibit-read-only t))
            (erase-buffer)
            (funcall fn))
          (ignore-errors
            (goto-char (point-min))
            (forward-line (1- line))
            (beginning-of-line))
          (dape--info-set-related-buffers))
        (when old-window
          (select-window old-window))))))

(defmacro dape--info-update-with (&rest body)
  "Create an update function from BODY.
See `dape--info-call-update-with'."
  (declare (indent 0))
  `(dape--info-call-update-with (lambda () ,@body)))

(defun dape--info-get-live-buffer (mode &optional index)
  "Get live dape info buffer with MODE and INDEX."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (dape--info-buffer-p mode index)))
            (dape--info-buffer-list)))

(defun dape--info-get-buffer-create (mode &optional index)
  "Get or create info buffer with MODE and INDEX."
  (with-current-buffer
      (or (dape--info-get-live-buffer mode index)
          (get-buffer-create (dape--info-buffer-name mode index)))
    (unless (eq major-mode mode)
      (funcall mode)
      (when index (setq dape--info-buffer-index index))
      (dape--info-set-related-buffers)
      (push (current-buffer) dape--info-buffers))
    (current-buffer)))

(defun dape-info-update ()
  "Update and display dape info buffers."
  (dolist (buffer (dape--info-buffer-list))
    (when (get-buffer-window buffer)
      (with-current-buffer buffer
        (revert-buffer)))))

(defun dape-info (&optional maybe-kill)
  "Create and display dape info buffers.
If MAYBE-KILL is non nil (which is always true when called
interactively) and all info buffers are already displayed, kill each
buffer info buffer.
See `dape-info-buffer-window-groups' for how to customize which
buffers get displayed and how they are grouped."
  (interactive (list t))
  (let (buffer-displayed-p)
    (cl-loop with displayed-buffers =
             (cl-remove-if-not #'get-buffer-window
                               (dape--info-buffer-list))
             for group in dape-info-buffer-window-groups unless
             (cl-loop for spec in group thereis
                      (cl-some (lambda (buffer)
                                 (with-current-buffer buffer
                                   (apply #'dape--info-buffer-p
                                          (ensure-list spec))))
                               displayed-buffers))
             do
             (setq buffer-displayed-p t)
             (dape--display-buffer
              (apply #'dape--info-get-buffer-create
                     (or
                      ;; Try to re-create the last window setup
                      (cl-find-if
                       (pcase-lambda (`(,hist-mode ,hist-index))
                         (cl-find-if
                          (pcase-lambda (`(,spec-mode ,spec-index))
                            (and (eq hist-mode spec-mode)
                                 (or (not spec-index)
                                     (eq hist-index spec-index))))
                          group
                          :key #'ensure-list))
                       dape--info-buffer-display-history)
                      ;; ...or fallback to car if no history
                      (ensure-list (car group))))))
    (when (and maybe-kill (not buffer-displayed-p))
      (cl-loop for buffer in (dape--info-buffer-list)
               do (kill-buffer buffer)))
    (dape-info-update)))

(defconst dape--info-buffer-name-alist
  '((dape-info-breakpoints-mode . "Breakpoints")
    (dape-info-threads-mode     . "Threads")
    (dape-info-stack-mode       . "Stack")
    (dape-info-modules-mode     . "Modules")
    (dape-info-sources-mode     . "Sources")
    (dape-info-watch-mode       . "Watch")
    (dape-info-scope-mode       . "Scope"))
  "Lookup for `dape-info-parent-mode' derived modes names.")

(defun dape--info-buffer-name (mode &optional index)
  "Return buffer name for MODE and INDEX."
  (concat (format "*dape-info %s*" (alist-get mode dape--info-buffer-name-alist))
          (when (and index (> index 0)) (format "<%s>" index))))

(defun dape--info-set-related-buffers ()
  "Set related buffers and `header-line-format'."
  (let* ((conn (dape--live-connection 'stopped t))
         (scopes (plist-get (dape--current-stack-frame conn) :scopes)))
    (when (or (not dape--info-buffer-related) scopes)
      (cl-loop for spec in (dape--info-window-group)
               for (mode index) = (ensure-list spec) append
               (cond ((and (eq 'dape-info-scope-mode mode) (not index))
                      (cl-loop for scope in scopes for index upfrom 0 collect
                               `(dape-info-scope-mode ,index ,(plist-get scope :name))))
                     ((and (eq 'dape-info-scope-mode mode) index)
                      (when-let* ((scope (nth index scopes)))
                        `((dape-info-scope-mode ,index ,(plist-get scope :name)))))
                     (`((,mode nil ,(alist-get mode dape--info-buffer-name-alist)))))
               into related
               finally (setq dape--info-buffer-related related))
      (cl-loop for (mode index name) in dape--info-buffer-related append
               `(,(if (dape--info-buffer-p mode index)
                      (dape--info-header name mode index nil nil 'mode-line)
                    (dape--info-header name mode index "mouse-1: select"
                                       'mode-line-highlight
                                       'mode-line-inactive))
                 " ")
               into format
               finally (setq header-line-format format)))))


;;; Info breakpoints buffer

(dape--command-at-line dape-info-breakpoint-disable (dape--info-breakpoint)
  "Enable/disable breakpoint at line in dape info buffer."
  (dape--breakpoint-disable
   dape--info-breakpoint (not (dape--breakpoint-disabled dape--info-breakpoint)))
  (dape--breakpoint-broadcast-update (dape--breakpoint-source dape--info-breakpoint))
  (revert-buffer)
  (run-hooks 'dape-update-ui-hook))

(dape--command-at-line dape-info-breakpoint-dwim (dape--info-breakpoint
                                                  dape--info-exception)
  "Toggle exception or goto breakpoint in info buffer."
  (cond (dape--info-breakpoint
         (with-selected-window
             (display-buffer
              (or (dape--breakpoint-buffer dape--info-breakpoint)
                  (find-file-noselect (dape--breakpoint-path dape--info-breakpoint)))
              dape-display-source-buffer-action)
           (goto-char (point-min))
           (forward-line (1- (dape--breakpoint-line dape--info-breakpoint)))))
        (dape--info-exception
         (plist-put dape--info-exception :enabled
                    (not (plist-get dape--info-exception :enabled)))
         (dolist (conn (dape--live-connections))
           (dape--set-exception-breakpoints conn))
         (revert-buffer)
         (run-hooks 'dape-update-ui-hook))))


(dape--command-at-line dape-info-breakpoint-delete (dape--info-breakpoint
                                                    dape--info-data-breakpoint)
  "Delete breakpoint at line in info buffer."
  (cond (dape--info-breakpoint
         (dape--breakpoint-remove dape--info-breakpoint))
        (dape--info-data-breakpoint
         (setq dape--data-breakpoints
               (delq dape--info-data-breakpoint
                     dape--data-breakpoints))
         (when-let* ((conn (dape--live-connection 'stopped t)))
           (dape--with-request (dape--set-data-breakpoints conn)))))
  (revert-buffer)
  (run-hooks 'dape-update-ui-hook))

(dape--command-at-line dape-info-breakpoint-log-edit (dape--info-breakpoint)
  "Edit breakpoint at line in dape info buffer."
  (with-selected-window
      (display-buffer
       (or (dape--breakpoint-buffer dape--info-breakpoint)
           (find-file-noselect (dape--breakpoint-path dape--info-breakpoint)))
       dape-display-source-buffer-action)
    (goto-char (point-min))
    (forward-line (1- (dape--breakpoint-line dape--info-breakpoint)))
    (call-interactively (pcase (dape--breakpoint-type dape--info-breakpoint)
                          ('log #'dape-breakpoint-log)
                          ('expression #'dape-breakpoint-expression)
                          ('hits #'dape-breakpoint-hits)
                          (_ (user-error "Unable to edit breakpoint on line \
without log or expression breakpoint"))))))

(dape--buffer-map dape-info-breakpoints-mode-line-map dape-info-breakpoint-dwim
  "D" #'dape-info-breakpoint-disable
  "d" #'dape-info-breakpoint-delete
  "e" #'dape-info-breakpoint-log-edit)

(defvar dape-info-breakpoints-mode-map
  (copy-keymap dape-info-breakpoints-mode-line-map))

(define-derived-mode dape-info-breakpoints-mode dape-info-parent-mode "Breakpoints"
  "Major mode for Dape info breakpoints."
  :interactive nil)

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-breakpoints-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-breakpoints-mode'."
  (dape--info-update-with
    (let ((table (make-gdb-table))
          (y (propertize "y" 'font-lock-face 'font-lock-warning-face))
          (n (propertize "n" 'font-lock-face 'font-lock-doc-face)))
      (cl-loop for plist in dape--data-breakpoints do
               (gdb-table-add-row
                table
                (list
                 y "Data "
                 (format "%s %s %s"
                         (propertize (plist-get plist :name)
                                     'font-lock-face
                                     'font-lock-variable-name-face)
                         (plist-get plist :accessType)
                         (when-let* ((data-id (plist-get plist :dataId)))
                           (format "(%s)" data-id))))
                (list 'dape--info-data-breakpoint plist)))
      (cl-loop for breakpoint in dape--breakpoints
               for line = (dape--breakpoint-line breakpoint)
               for verified-plist = (dape--breakpoint-verified breakpoint)
               for verified-p = (or
                                 ;; No live connection show all as verified
                                 (not (dape--live-connection 'last t))
                                 ;; Actually verified by any connection
                                 (cl-find-if (apply-partially #'plist-get
                                                              verified-plist)
                                             (dape--live-connections))
                                 ;; If hit then must be verified
                                 (dape--breakpoint-hits breakpoint))
               do
               (gdb-table-add-row
                table
                (list
                 (cond ((dape--breakpoint-disabled breakpoint) n)
                       ((when-let* ((hits (dape--breakpoint-hits breakpoint)))
                          (propertize (format "%s" hits)
                                      'font-lock-face 'font-lock-warning-face)))
                       (y))
                 (pcase (dape--breakpoint-type breakpoint)
                   ('log        "Log  ")
                   ('hits       "Hits ")
                   ('expression "Cond ")
                   ('until      "Until")
                   (_           "Break"))
                 (or
                  ;; If buffer live, display part of the line
                  (when-let* ((buffer (dape--breakpoint-buffer breakpoint)))
                    (concat
                     (if-let* ((file (buffer-file-name buffer)))
                         (dape--format-file-line file line)
                       (format "%s:%d" (buffer-name buffer) line))
                     (concat
                      " "
                      (thread-first
                        (dape--with-line buffer line
                          (or (thing-at-point 'line) ""))
                        (string-trim-right)
                        (truncate-string-to-width 80 nil nil t)))))
                  ;; Otherwise just show path:line
                  (when-let* ((path (dape--breakpoint-path breakpoint)))
                    (dape--format-file-line path line))))
                `( dape--info-breakpoint ,breakpoint
                   mouse-face highlight
                   help-echo "mouse-2, RET: visit breakpoint"
                   ,@(unless verified-p '(font-lock-face shadow)))))
      (cl-loop for exception in dape--exceptions do
               (gdb-table-add-row
                table
                (list (if (plist-get exception :enabled) y n)
                      "Excep"
                      (format "%s" (plist-get exception :label)))
                (list 'dape--info-exception exception
                      'mouse-face 'highlight
                      'help-echo "mouse-2, RET: toggle exception")))
      (insert (gdb-table-string table " ")))))


;;; Info threads buffer

(defvar dape--info-thread-position nil
  "`dape-info-thread-mode' marker for `overlay-arrow-variable-list'.")
(defvar-local dape--info-threads-skip-other-p nil
  ;; XXX Some adapters bork on parallel stack traces
  "If non nil skip fetching thread information for other threads.")
(defvar dape-info--threads-tt-bench 2
  "Time to Bench.")

(dape--command-at-line dape-info-select-thread (dape--info-thread dape--info-conn)
  "Select thread at line in dape info buffer."
  (dape-select-thread dape--info-conn (plist-get dape--info-thread :id))
  (revert-buffer))

(defvar dape--info-threads-font-lock-keywords
  '(("in \\([^ ^(]+\\)"  (1 font-lock-function-name-face))
    (" \\(unknown\\)"  (1 font-lock-warning-face))
    (" \\(stopped\\)"  (1 font-lock-warning-face))
    (" \\(exited\\)"  (1 font-lock-warning-face))
    (" \\(running\\)"  (1 font-lock-string-face))
    (" \\(started\\)"  (1 font-lock-string-face)))
  "Keywords for `dape-info-threads-mode'.")

(dape--buffer-map dape-info-threads-mode-line-map dape-info-select-thread
  ;; TODO Add bindings for individual threads.
  )

(defvar dape-info-threads-mode-map
  (copy-keymap dape-info-threads-mode-line-map))

(defun dape--info-threads-stack-info (conn cb)
  "Populate stack frame info for CONNs threads.
See `dape-request' for expected CB signature."
  (let (threads)
    (cond
     ;; Current CONN is benched
     (dape--info-threads-skip-other-p
      (dape--request-continue cb))
     ;; Stopped threads
     ((setq threads
            (cl-remove-if (lambda (thread)
                            (plist-get thread :request-in-flight))
                          (dape--stopped-threads conn)))
      (let ((start-time (current-time))
            (responses 0))
        (dolist (thread threads)
          ;; Keep track of requests in flight as `revert-buffer' might
          ;; be called at any time, and we want keep unnecessary
          ;; chatter at a minimum.
          (plist-put thread :request-in-flight t)
          (dape--with-request (dape--stack-trace conn thread 1)
            (plist-put thread :request-in-flight nil)
            ;; Time response, if slow skip these kind of requests in
            ;; the future (saving state in buffer local variable)
            (when (and (not dape--info-threads-skip-other-p)
                       (time-less-p (timer-relative-time
                                     start-time dape-info--threads-tt-bench)
                                    (current-time)))
              (dape--warn "Disabling stack info for other threads (slow)")
              (setq dape--info-threads-skip-other-p t))
            ;; When all request have resolved return
            (when (length= threads (setf responses (1+ responses)))
              (dape--request-continue cb))))))
     ;; No stopped threads
     (t (dape--request-continue cb)))))

(define-derived-mode dape-info-threads-mode dape-info-parent-mode "Threads"
  "Major mode for dape info threads."
  :interactive nil
  (setq font-lock-defaults '(dape--info-threads-font-lock-keywords)
        truncate-lines nil
        dape--info-thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'dape--info-thread-position))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-threads-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-threads-mode'."
  (if-let* ((conn (dape--live-connection 'last t))
            ((dape--threads conn)))
      (dape--with-request (dape--info-threads-stack-info conn)
        (cl-loop
         initially do (set-marker dape--info-thread-position nil)
         with table = (make-gdb-table)
         with conns = (dape--live-connections)
         with current-thread = (dape--current-thread conn)
         with line = 0
         with selected-line
         for conn in conns
         for index upfrom 1 do
         (cl-loop
          for thread in (dape--threads conn) do
          (cl-incf line)
          (when (eq current-thread thread) (setq selected-line line))
          (gdb-table-add-row
           table
           (list
            (format "%s" line)
            (concat
             (plist-get thread :name)
             " "
             (if-let* ((status (plist-get thread :status)))
                 (format "%s" status)
               "unknown")
             (if-let* (((equal (plist-get thread :status) 'stopped))
                       (top-stack (car (plist-get thread :stackFrames))))
                 (concat
                  " in " (plist-get top-stack :name)
                  (when-let* ((dape-info-thread-buffer-locations)
                              (path (thread-first top-stack
                                                  (plist-get :source)
                                                  (plist-get :path)))
                              (path (dape--path-local conn path))
                              (line (plist-get top-stack :line)))
                    (concat " of " (dape--format-file-line path line)))
                  (when-let* ((dape-info-thread-buffer-addresses)
                              (addr
                               (plist-get top-stack :instructionPointerReference)))
                    (concat " at " addr))
                  " "))))
           (list 'dape--info-conn conn
                 'dape--info-thread thread
                 'dape--selected (eq current-thread thread)
                 'mouse-face 'highlight
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
  '(("^[ 0-9]+ \\([^ ^(]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--command-at-line dape-info-stack-select (dape--info-frame)
  "Select stack at line in dape info buffer."
  (dape-select-stack (dape--live-connection 'stopped)
                     (plist-get dape--info-frame :id))
  (revert-buffer))

(dape--command-at-line dape-info-stack-memory (dape--info-frame)
  "View and edit memory at address of frame."
  (if-let* ((ref (plist-get dape--info-frame :instructionPointerReference)))
      (dape-memory ref)
    (user-error "No address for frame")))

(dape--command-at-line dape-info-stack-disassemble (dape--info-frame)
  "View disassemble at address of frame."
  (if-let* ((address (plist-get dape--info-frame :instructionPointerReference)))
      (dape-disassemble address)
    (user-error "No address for frame")))

(dape--buffer-map dape-info-stack-mode-line-map dape-info-stack-select
  "m" #'dape-info-stack-memory
  "M" #'dape-info-stack-disassemble
  "D" #'dape-info-stack-disassemble)

(defvar dape-info-stack-mode-map (copy-keymap dape-info-stack-mode-line-map))

(define-derived-mode dape-info-stack-mode dape-info-parent-mode "Stack"
  "Major mode for Dape info stack."
  :interactive nil
  (setq font-lock-defaults '(dape--info-stack-font-lock-keywords)
        dape--info-stack-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'dape--info-stack-position))

(defun dape--info-stack-buffer-insert (conn current-stack-frame stack-frames)
  "Helper for inserting stack info into stack buffer.
Create table from CURRENT-STACK-FRAME and STACK-FRAMES and insert into
current buffer with CONN config."
  (cl-loop with table = (make-gdb-table) with selected-line
           for line from 1 for frame in stack-frames do
           (when (eq current-stack-frame frame)
             (setq selected-line line))
           (gdb-table-add-row
            table
            (list
             (format "%s" line)
             (concat
              (plist-get frame :name)
              (when-let* ((dape-info-stack-buffer-locations)
                          (path (thread-first frame
                                              (plist-get :source)
                                              (plist-get :path)))
                          (path (dape--path-local conn path)))
                (concat " of "
                        (dape--format-file-line path (plist-get frame :line))))
              (when-let* ((dape-info-stack-buffer-addresses)
                          (ref (plist-get frame :instructionPointerReference)))
                (concat " at " ref))
              " "))
            (list 'dape--info-frame frame
                  'dape--selected (eq current-stack-frame frame)
                  'mouse-face 'highlight
                  'help-echo "mouse-2, RET: select frame"))
           finally do
           (insert (gdb-table-string table " "))
           (when selected-line
             (gdb-mark-line selected-line dape--info-stack-position))))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-stack-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-stack-mode'."
  (let* ((conn (or (dape--live-connection 'stopped t t)
                   (dape--live-connection 'last t t)))
         (current-thread (dape--current-thread conn))
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
          (insert "No stack information available.")))))
     (;; Only one frame are guaranteed to be available due to
      ;; `supportsDelayedStackTraceLoading' optimizations
      (dape--with-request
          (dape--stack-trace conn current-thread dape-stack-trace-levels)
        ;; If stack trace lookup with `dape-stack-trace-levels' frames changed
        ;; the stack frame list, we need to update the buffer again
        (dape--info-update-with
          (dape--info-stack-buffer-insert conn current-stack-frame
                                          (plist-get current-thread :stackFrames))))))))


;;; Info modules buffer

(defvar dape--info-modules-font-lock-keywords
  '(("^No" (1 default)) ;; Skip fontification of placeholder string
    ("^\\([^ ]+\\) "  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(dape--command-at-line dape-info-modules-goto (dape--info-module)
  "Goto module."
  (let ((conn (dape--live-connection 'last t))
        (source (list :source dape--info-module)))
    (dape--with-request (dape--source-ensure conn source)
      (if-let* ((marker
                 (dape--object-to-marker conn source)))
          (pop-to-buffer (marker-buffer marker))
        (user-error "Unable to open module")))))

(dape--buffer-map dape-info-modules-mode-line-map dape-info-modules-goto)

(defvar dape-info-modules-mode-map
  (copy-keymap dape-info-modules-mode-line-map))

(define-derived-mode dape-info-modules-mode dape-info-parent-mode "Modules"
  "Major mode for Dape info modules."
  :interactive nil
  (setq font-lock-defaults '(dape--info-modules-font-lock-keywords))
  (dape--info-update-with
    (insert "No modules available.")))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-modules-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-modules-mode'."
  ;; Use last connection if current is dead
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t)
                        dape--connection))
              (modules (dape--modules conn)))
    (dape--info-update-with
      (cl-loop with table = (make-gdb-table)
               for module in (reverse modules) do
               (gdb-table-add-row
                table
                (list
                 (concat
                  (plist-get module :name)
                  (when-let* ((path (plist-get module :path)))
                    (concat " of " (dape--format-file-line path nil)))
                  (when-let* ((address-range (plist-get module :addressRange)))
                    (concat " at " address-range nil))
                  " "))
                (list 'dape--info-module module
                      'mouse-face 'highlight
                      'help-echo (format "mouse-2: goto module")))
               finally (insert (gdb-table-string table " "))))))


;;; Info sources buffer

(dape--command-at-line dape-info-sources-goto (dape--info-source)
  "Goto source."
  (let ((conn (dape--live-connection 'last t))
        (source (list :source dape--info-source)))
    (dape--with-request (dape--source-ensure conn source)
      (if-let* ((marker
                 (dape--object-to-marker conn source)))
          (pop-to-buffer (marker-buffer marker))
        (user-error "Unable to get source")))))

(dape--buffer-map dape-info-sources-mode-line-map dape-info-sources-goto)

(defvar dape-info-sources-mode-map
  (copy-keymap dape-info-sources-mode-line-map))

(define-derived-mode dape-info-sources-mode dape-info-parent-mode "Sources"
  "Major mode for Dape info sources."
  :interactive nil
  (dape--info-update-with
    (insert "No sources available.")))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-sources-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-sources-mode'."
  ;; Use last connection if current is dead
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t)
                        dape--connection))
              (sources (dape--sources conn)))
    (dape--info-update-with
      (cl-loop with table = (make-gdb-table)
               for source in (reverse sources) do
               (gdb-table-add-row
                table (list (concat (plist-get source :name) " "))
                (list 'dape--info-source source
                      'mouse-face 'highlight
                      'help-echo "mouse-2, RET: goto source"))
               finally (insert (gdb-table-string table " "))))))


;;; Info scope buffer

(defvar dape--variable-expanded-p (make-hash-table :test 'equal)
  "Hash table to keep track of expanded info variables.")

(defun dape--variable-expanded-p (path)
  "If PATH should be expanded."
  (gethash path dape--variable-expanded-p
           (when-let* ((auto-expand
                        ;; See `dape-variable-auto-expand-alist'.
                        ;; Expects car of PATH to specify context
                        (or (alist-get (car (last path)) dape-variable-auto-expand-alist)
                            (alist-get nil dape-variable-auto-expand-alist))))
             (length< path (+ auto-expand 2)))))

(dape--command-at-line dape-info-scope-toggle (dape--info-path)
  "Expand or contract variable at line in dape info buffer."
  (unless (dape--live-connection 'stopped)
    (user-error "No stopped threads"))
  (puthash dape--info-path (not (dape--variable-expanded-p dape--info-path))
           dape--variable-expanded-p)
  (revert-buffer))

(dape--buffer-map dape-info-variable-prefix-map dape-info-scope-toggle)

(dape--command-at-line dape-info-scope-watch-dwim (dape--info-variable)
  "Watch variable or remove from watch at line in dape info buffer."
  (dape-watch-dwim `(,(or (plist-get dape--info-variable :evaluateName)
                          (plist-get dape--info-variable :name)))
                   (eq major-mode 'dape-info-watch-mode)
                   (eq major-mode 'dape-info-scope-mode))
  (revert-buffer))

(dape--buffer-map dape-info-variable-name-map dape-info-scope-watch-dwim)

(dape--command-at-line dape-info-variable-edit (dape--info-ref dape--info-variable)
  "Edit variable value at line in dape info buffer."
  (dape--set-variable
   (dape--live-connection 'stopped) dape--info-ref dape--info-variable
   (read-string (format "Set value of %s `%s' = "
                        (plist-get dape--info-variable :type)
                        (plist-get dape--info-variable :name))
                nil nil (or (plist-get dape--info-variable :value)
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
        ((&key dataId description accessTypes &allow-other-keys) error)
        (dape-request conn :dataBreakpointInfo
                      (if (numberp dape--info-ref)
                          (list :variablesReference dape--info-ref
                                :name name)
                        (list :name name
                              :frameId (plist-get (dape--current-stack-frame conn) :id))))
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

(defvar dape-info-scope-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'dape-info-scope-toggle)
    (define-key map "W" #'dape-info-scope-watch-dwim)
    (define-key map "=" #'dape-info-variable-edit)
    (define-key map "b" #'dape-info-scope-data-breakpoint)
    map)
  "Keymap for buffers displaying variables.")

(defvar dape-info-scope-mode-map
  (copy-keymap dape-info-scope-mode-line-map))

(defun dape--info-locals-table-columns-list (alist)
  "Format and arrange the columns in locals display based on ALIST."
  ;; Stolen from gdb-mi but reimpleted due to usage of dape customs
  ;; org function `gdb-locals-table-columns-list'.
  (let (columns)
    (dolist (config dape-info-variable-table-row-config columns)
      (let* ((key (car config))
             (max (cdr config))
             (prop-org (alist-get key alist))
             (prop prop-org))
        (when prop-org
          (setq prop (substring prop 0 (string-match-p "\n" prop)))
          (if (and (> max 0) (length> prop max))
              (push (propertize (string-truncate-left prop max) 'help-echo prop-org)
                    columns)
            (push prop columns)))))
    (nreverse columns)))

(defun dape--info-scope-add-variable (table object ref path test-expanded
                                            &optional no-handles)
  "Add variable OBJECT with REF and PATH to TABLE.
TEST-EXPANDED is called with PATH and OBJECT to determine if recursive
calls should continue.  If NO-HANDLES is non nil skip + - handles."
  (let* ((name (or (plist-get object :name) ""))
         (type (or (plist-get object :type) ""))
         (value (or (plist-get object :value)
                    (plist-get object :result)
                    " "))
         (prefix (make-string (* (1- (length path)) 2) ?\s))
         (path (cons name path))
         (expanded-p (funcall test-expanded path))
         row)
    (setq
     name (propertize name
                      'font-lock-face 'font-lock-variable-name-face
                      'mouse-face 'highlight
                      'help-echo "mouse-2: create or remove watch expression"
                      'keymap dape-info-variable-name-map)
     type (propertize type 'font-lock-face 'font-lock-type-face)
     value (propertize value
                       'mouse-face 'highlight
                       'help-echo "mouse-2: edit value"
                       'keymap dape-info-variable-value-map)
     prefix (cond (no-handles prefix)
                  ((zerop (or (plist-get object :variablesReference) 0))
                   (concat prefix "  "))
                  ((and expanded-p (plist-get object :variables))
                   (concat
                    (propertize (concat prefix "-")
                                'mouse-face 'highlight
                                'help-echo "mouse-2: contract"
                                'keymap dape-info-variable-prefix-map)
                    " "))
                  ((concat
                    (propertize (concat prefix "+")
                                'mouse-face 'highlight
                                'help-echo "mouse-2: expand"
                                'keymap dape-info-variable-prefix-map)
                    " ")))
     row (dape--info-locals-table-columns-list `((name  . ,name)
                                                 (type  . ,type)
                                                 (value . ,value))))
    (setcar row (concat prefix (car row)))
    (gdb-table-add-row table
                       (if dape-info-variable-table-aligned
                           row
                         (list (mapconcat #'identity row " ")))
                       (list 'dape--info-variable object
                             'dape--info-path path
                             ;; `dape--command-at-line' expects non nil
                             'dape--info-ref (or ref 'refless)))
    (when expanded-p
      ;; TODO Should be paged
      (dolist (variable (plist-get object :variables))
        (dape--info-scope-add-variable table variable
                                       (plist-get object :variablesReference)
                                       path test-expanded no-handles)))))

;; FIXME Empty header line when adapter is killed
(define-derived-mode dape-info-scope-mode dape-info-parent-mode "Scope"
  "Major mode for Dape info scope."
  :interactive nil
  (setq dape--info-buffer-index 0)
  (dape--info-update-with (insert "No scope information available.")))

(cl-defmethod dape--info-revert (&context (major-mode (eql dape-info-scope-mode))
                                          &optional _ignore-auto _noconfirm _preserve-modes)
  "Revert buffer function for MAJOR-MODE `dape-info-scope-mode'."
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t)))
              (frame (dape--current-stack-frame conn))
              (scopes (plist-get frame :scopes))
              ;; FIXME Scope list could have shrunk and
              ;; `dape--info-buffer-index' can be out of bounds.
              (scope (nth dape--info-buffer-index scopes))
              ;; Check for stopped threads to reduce flickering
              ((dape--stopped-threads conn)))
    (dape--with-request (dape--variables conn scope)
      (dape--with-request
          (dape--variables-recursive conn scope
                                     (list dape--info-buffer-index)
                                     #'dape--variable-expanded-p)
        (when (and scope scopes (dape--stopped-threads conn))
          (dape--info-update-with
            (cl-loop
             with table = (make-gdb-table)
             for object in (plist-get scope :variables)
             initially do
             (setf (gdb-table-right-align table)
                   dape-info-variable-table-aligned)
             do
             (dape--info-scope-add-variable table
                                            object
                                            (plist-get scope :variablesReference)
                                            (list dape--info-buffer-index)
                                            #'dape--variable-expanded-p)
             finally (insert (gdb-table-string table " ")))))))))


;;; Info watch buffer

(defvar dape-info-watch-mode-line-map (copy-keymap dape-info-scope-mode-line-map))

(defvar dape-info-watch-mode-map
  (let ((map (make-composed-keymap (copy-keymap dape-info-watch-mode-line-map))))
    (define-key map "\C-x\C-q" #'dape-info-watch-edit-mode)
    map))

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
                                             '(watch)
                                             #'dape--variable-expanded-p)
                (dape--info-update-with
                  (cl-loop with table = (make-gdb-table)
                           for watch in dape--watched
                           initially (setf (gdb-table-right-align table)
                                           dape-info-variable-table-aligned)
                           do
                           (dape--info-scope-add-variable table watch nil '(watch)
                                                          #'dape--variable-expanded-p)
                           finally (insert (gdb-table-string table " "))))))))))
     (t
      (dape--info-update-with
        (cl-loop with table = (make-gdb-table)
                 for watch in dape--watched
                 initially (setf (gdb-table-right-align table)
                                 dape-info-variable-table-aligned)
                 do
                 (dape--info-scope-add-variable table watch nil '(watch)
                                                #'dape--variable-expanded-p)
                 finally (insert (gdb-table-string table " "))))))))

(defvar dape--info-watch-edit-font-lock-keywords
  '(("\\(.+\\)"  (1 font-lock-variable-name-face))))

(defvar dape-info-watch-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" #'dape-info-watch-finish-edit)
    (define-key map "\C-c\C-k" #'dape-info-watch-abort-changes)
    map)
  "Local keymap for dape watch buffer in edit mode.")

(define-derived-mode dape-info-watch-edit-mode dape-info-watch-mode "Watch Edit"
  "Major mode for editing watch info."
  (set-buffer-modified-p nil)
  (setq revert-buffer-function #'dape--info-revert
        buffer-undo-list nil
        buffer-read-only nil
        font-lock-defaults '(dape--info-watch-edit-font-lock-keywords))
  (message "%s" (substitute-command-keys
	         "Press \\[dape-info-watch-finish-edit] when finished \
or \\[dape-info-watch-abort-changes] to abort changes"))
  (dape--info-set-related-buffers)
  (revert-buffer))

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
  (dape--info-set-related-buffers)
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

(defvar dape--repl-marker nil
  "`dape-repl-mode' marker for `overlay-arrow-variable-list'.")

(defun dape--repl-insert (string)
  "Insert STRING into REPL.
If REPL buffer is not live STRING will be displayed in minibuffer."
  (when (stringp string)
    (if-let* ((buffer (get-buffer "*dape-repl*")))
        (with-current-buffer buffer
          (save-excursion
            (let (start)
              (if comint-last-prompt
                  (goto-char (marker-position (car comint-last-prompt)))
                (goto-char (point-max)))
              (setq start (point-marker))
              (let ((inhibit-read-only t))
                (insert string))
              ;; XXX Inserting at position of `comint-last-prompt'...
              (when comint-last-prompt
                (move-marker (car comint-last-prompt) (point)))
              ;; ...and process marker forcing us to move marker by hand.
              (when-let* ((process (get-buffer-process buffer)))
                (set-marker (process-mark process)
                            (+ (point) (length dape--repl-prompt))))
              ;; HACK Run hooks as if `comint-output-filter' was executed
              (let ((comint-last-output-start start))
                (run-hook-with-args 'comint-output-filter-functions string)))))
      ;; Fallback to `message' if no repl buffer
      (message (string-trim string)))))

(defun dape--repl-insert-error (string)
  "Insert STRING into REPL with error face."
  (dape--repl-insert (propertize string 'font-lock-face 'dape-repl-error-face)))

(defun dape--repl-insert-prompt ()
  "Insert `dape--repl-insert-prompt' into repl."
  (when-let* ((buffer (get-buffer "*dape-repl*"))
              (dummy-process (get-buffer-process buffer)))
    (comint-output-filter dummy-process dape--repl-prompt)))

(defun dape--repl-move-marker (point)
  "Mark the first line containing text property `dape--selected'.
The search is done backwards from POINT.  The line is marked with
`dape--repl-marker' and `gdb-mark-line'."
  (save-excursion
    (goto-char point)
    (when (text-property-search-backward 'dape--selected)
      (gdb-mark-line (line-number-at-pos) dape--repl-marker))))

(defun dape--repl-revert-region (&rest _)
  "Revert region by cont text property dape--revert-tag."
  (when-let* ((fn (get-text-property (point) 'dape--revert-fn))
              (start (save-excursion
                       (previous-single-property-change
                        (1+ (point)) 'dape--revert-tag)))
              (end (save-excursion
                     (next-single-property-change
                      (point) 'dape--revert-tag))))
    (let ((line (line-number-at-pos (point) t))
          (col (current-column)))
      (delete-region start end)
      (insert (funcall fn))
      (dape--repl-move-marker (1+ (point)))
      (ignore-errors
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char col)))))

(defun dape--repl-make-region-string (str revert-function keymap)
  "Return STR with local REVERT-FUNCTION and KEYMAP."
  (cl-loop for (start end props) in (object-intervals str) do
           (add-text-properties start end
                                `( keymap ,(make-composed-keymap
                                            (list (plist-get props 'keymap) keymap))
                                   font-lock-face ,(or (plist-get props 'font-lock-face)
                                                       (plist-get props 'face))
                                   face nil)
                                str)
           finally return
           (propertize str
                       'dape--revert-tag (gensym "dape-region-tag")
                       'dape--revert-fn revert-function)))

(defun dape--repl-variable (variable)
  "Return VARIABLE string representation with CONN."
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t))))
    (let ((dape--request-blocking t))
      (dape--variables conn variable #'ignore)
      (dape--variables-recursive conn variable `(,(plist-get variable :name) repl)
                                 #'dape--variable-expanded-p #'ignore)))
  (let ((table (make-gdb-table)))
    (setf (gdb-table-right-align table) dape-info-variable-table-aligned)
    (dape--info-scope-add-variable table variable nil '(repl) #'dape--variable-expanded-p)
    (dape--repl-make-region-string (gdb-table-string table " ")
                                   (apply-partially #'dape--repl-variable variable)
                                   dape-info-scope-mode-line-map)))

(defun dape--repl-info-string (mode index)
  "Return info MODE buffer content as string.
See `dape--info-buffer-index' for information on INDEX."
  (with-temp-buffer
    (funcall mode)
    (setq dape--info-buffer-index index)
    (let ((dape-ui-debounce-time 0)
          (dape--request-blocking t))
      (revert-buffer))
    (ignore-errors (font-lock-ensure))
    (dape--repl-make-region-string
     (buffer-substring (point-min) (point-max))
     (apply-partially #'dape--repl-info-string mode index)
     (symbol-value (intern (concat (symbol-name mode) "-line-map"))))))

(defun dape--repl-insert-info-buffer (mode &optional index)
  "Insert content of MODE info buffer into repl.
See `dape--repl-info-string' for information on INDEX."
  (dape--repl-insert (concat (dape--repl-info-string mode index) "\n"))
  (when-let* ((buffer (get-buffer "*dape-repl*")))
    (with-current-buffer buffer
      (dape--repl-move-marker (point-max)))))

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
  "Send INPUT to DUMMY-PROCESS.
Called by `comint-input-sender' in `dape-repl-mode'."
  (setq input (string-trim-right input "[\n\r]+"))
  (cond
   ;; Run previous input
   ((and (string-empty-p input)
         (not (string-empty-p (car (ring-elements comint-input-ring)))))
    (when-let* ((last (car (ring-elements comint-input-ring))))
      (message "Using last input `%s'" last)
      (dape--repl-input-sender dummy-process last)))
   ;; Run command from `dape-named-commands'
   ((pcase-let* ((`(,cmd . ,args)
                  (split-string (substring-no-properties input)
                                split-string-default-separators))
                 (fn (or (alist-get cmd dape-repl-commands nil nil #'equal)
                         (and dape-repl-use-shorthand
                              (cdr (assoc cmd (dape--repl-shorthand-alist)))))))
      (cond ((eq 'dape-quit fn)
             ;; HACK: `comint-send-input' expects buffer to be live
             ;;       on `comint-input-sender' return.
             (run-with-timer 0 nil #'call-interactively #'dape-quit))
            ((and (commandp fn) args) nil)
            ((commandp fn)
             (dape--repl-insert-prompt)
             (call-interactively fn)
             t)
            (fn
             (dape--repl-insert-prompt)
             (condition-case-unless-debug err
                 (apply fn args)
               (error (dape--warn "%s" (car err))))
             t))))
   ;; Evaluate expression
   (t
    (dape--repl-insert-prompt)
    (dape-evaluate-expression
     (or (dape--live-connection 'stopped t)
         (dape--live-connection 'last))
     (string-trim (substring-no-properties input))))))

(defun dape--repl-completion-at-point ()
  "Completion at point function for *dape-repl* buffer."
  (when-let* ((conn (or (dape--live-connection 'stopped t)
                        (dape--live-connection 'last t)))
              ((dape--capable-p conn :supportsCompletionsRequest)))
    (let* ((line-start (comint-line-beginning-position))
           (str (buffer-substring-no-properties line-start (point-max)))
           (column (1+ (- (point) line-start)))
           (bounds (or (bounds-of-thing-at-point 'word)
                       (cons (point) (point))))
           (trigger-chars
            (or (thread-first conn
                              (dape--capabilities)
                              ;; completionTriggerCharacters is an
                              ;; unofficial array of string to trigger
                              ;; completion on.
                              (plist-get :completionTriggerCharacters)
                              (append nil))
                '(".")))
           (collection
            (when (and (derived-mode-p 'dape-repl-mode)
                       ;; Add `dape-repl-commands' if completion
                       ;; starts at beginning of prompt line.
                       (eql (comint-line-beginning-position) (car bounds)))
              (cl-loop
               with alist = (append dape-repl-commands
                                    (when dape-repl-use-shorthand
                                      (dape--repl-shorthand-alist)))
               for (name . cmd) in alist
               for anno = (propertize (symbol-name cmd)
                                      'face 'font-lock-builtin-face)
               collect `( ,name . ,(concat " " anno)))))
           done)
      (dape--with-request-bind
          ((&key targets &allow-other-keys) _error)
          (dape-request
           conn :completions
           `( :text ,str
              :column ,column
              ,@(when (dape--stopped-threads conn)
                  `(:frameId
                    ,(plist-get (dape--current-stack-frame conn) :id)))))
        (setf collection
              (append
               collection
               (mapcar
                (lambda (target)
                  (cons
                   (substring
                    (or (plist-get target :text) (plist-get target :label))
                    (when-let* ((start (plist-get target :start))
                                (offset (- (car bounds) line-start))
                                ((< start offset)))
                      ;; XXX Adapter gets line but Emacs completion is
                      ;; given `word' bounds, cut prefix off candidate
                      ;; such that it matches the bounds.
                      (- offset start)))
                   (concat
                    (when-let* ((type (plist-get target :type)))
                      (concat " " (propertize type 'face 'font-lock-type-face)))
                    (when-let* ((detail (plist-get target :detail)))
                      (concat " " (propertize detail 'face 'font-lock-doc-face))))))
                targets))
              done t))
      (while-no-input
        (while (not done) (accept-process-output nil 0 1)))
      (list (car bounds) (cdr bounds) collection
            :annotation-function
            (lambda (str) (cdr (assoc (substring-no-properties str) collection)))
            :company-prefix-length
            (save-excursion
              (goto-char (car bounds))
              (looking-back (regexp-opt trigger-chars) line-start))))))

(defun dape-repl-threads (&optional index)
  "List threads in *dape-repl* buffer.
If INDEX is non nil parse into number and select n+1th thread."
  (when-let* ((index (and index (string-to-number index))))
    (cl-loop with n = 0 for conn in (dape--live-connections)
             for thread = (cl-loop for thread in (dape--threads conn)
                                   when (equal (cl-incf n) index) return thread)
             when thread return (dape-select-thread conn (plist-get thread :id))))
  (dape--repl-insert-info-buffer 'dape-info-threads-mode))

(defun dape-repl-stack (&optional index)
  "List modules in *dape-repl* buffer.
If INDEX is non nil parse into number and select n+1th stack."
  (when-let* ((index (and index (string-to-number index)))
              (conn (dape--live-connection 'stopped t))
              (frames (plist-get (dape--current-thread conn) :stackFrames)))
    (dape-select-stack conn (plist-get (nth (1- index) frames) :id)))
  (dape--repl-insert-info-buffer 'dape-info-stack-mode))

(defun dape-repl-modules ()
  "List modules in *dape-repl* buffer."
  (dape--repl-insert-info-buffer 'dape-info-modules-mode))

(defun dape-repl-sources ()
  "List sources in *dape-repl* buffer."
  (dape--repl-insert-info-buffer 'dape-info-sources-mode))

(defun dape-repl-breakpoints ()
  "List breakpoints in *dape-repl* buffer."
  (dape--repl-insert-info-buffer 'dape-info-breakpoints-mode))

(defun dape-repl-scope (&optional index)
  "List variables of scope in *dape-repl* buffer.
If INDEX is non nil parse into number and show n+1th scope."
  (dape--repl-insert-info-buffer 'dape-info-scope-mode
                                 (string-to-number (or index ""))))

(defun dape-repl-watch (&rest expressions)
  "List watched variables in *dape-repl* buffer.
If EXPRESSIONS is non blank add or remove expression to watch list."
  (when expressions
    (dape-watch-dwim `(,(string-join expressions " "))))
  (dape--repl-insert-info-buffer 'dape-info-watch-mode))

(define-derived-mode dape-repl-mode comint-mode "REPL"
  "Mode for *dape-repl* buffer."
  :group 'dape
  :interactive nil
  (setq-local revert-buffer-function #'dape--repl-revert-region
              dape--repl-marker (make-marker)
              comint-prompt-read-only t
              comint-scroll-to-bottom-on-input t
              ;; Always keep prompt at the bottom of the window
              scroll-conservatively 101
              comint-input-sender 'dape--repl-input-sender
              comint-prompt-regexp (concat "^" (regexp-quote dape--repl-prompt))
              comint-process-echoes nil)
  (add-to-list 'overlay-arrow-variable-list 'dape--repl-marker)
  (add-hook 'completion-at-point-functions
            #'dape--repl-completion-at-point nil t)
  ;; Stolen from ielm
  ;; Start a dummy process just to please comint
  (unless (comint-check-proc (current-buffer))
    (let ((process (start-process "dape repl" (current-buffer) nil)))
      (add-hook 'kill-buffer-hook (lambda () (delete-process process)) nil t))
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer))
                                    nil)
    (set-process-filter (get-buffer-process (current-buffer))
                        #'comint-output-filter)
    (insert
     (format
      "* Welcome to the Dape REPL *

Available Dape commands:
%s

Any other input or input starting with a space is sent directly to the
debugger.  An empty line will repeat the last command.\n\n"
      (with-temp-buffer
        (insert  "  "
                 (mapconcat (pcase-lambda (`(,str . ,command))
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
                            ", "))
        (let ((fill-column 72)
              (adaptive-fill-mode t))
          (fill-region (point-min) (point-max)))
        (buffer-string))))
    (set-marker (process-mark (get-buffer-process (current-buffer))) (point))
    (comint-output-filter (get-buffer-process (current-buffer))
                          dape--repl-prompt)))

(defun dape-repl ()
  "Create or select *dape-repl* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*dape-repl*")
    (unless (eq major-mode 'dape-repl-mode)
      (dape-repl-mode))
    (let ((window (dape--display-buffer (current-buffer))))
      (when (called-interactively-p 'interactive)
        (select-window window)))))


;;; Inlay hints

(defface dape-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay-hint overlays.")

(defface dape-inlay-hint-highlight-face '((t (:height 0.8 :inherit highlight)))
  "Face used for highlighting parts of inlay-hint overlays.")

(defvar dape--inlay-hint-overlays nil "List of all hint overlays.")
(defvar dape--inlay-hint-debounce-timer (timer-create) "Debounce timer.")
(defvar dape--inlay-hint-symbols-fn #'dape--inlay-hint-collect-symbols
  "Function returning variable names.")
(defvar dape--inlay-hint-seperator (propertize " | " 'face 'dape-inlay-hint-face)
  "Hint delimiter.")

(defun dape--inlay-hint-collect-symbols (start end)
  "Return list of variable symbol candidates between START and END.
Excludes symbols that are part of strings, comments or documentation."
  (unless (<= (- end start) 300)
    ;; Clamp the region size to prevent performance issues
    (setq end (+ start 300)))
  (save-excursion
    (goto-char start)
    (cl-loop for symbol = (thing-at-point 'symbol)
             when (and symbol
                       ;; Skip symbols in strings, comments, or docstrings
                       (not (memql (get-text-property 0 'face symbol)
                                   '(font-lock-string-face
                                     font-lock-doc-face
                                     font-lock-comment-face))))
             collect (list symbol) into symbol-list
             for previous-point = (point)
             do (forward-thing 'symbol)
             while (and (< previous-point (point))
                        (<= (point) end))
             finally return (delete-dups symbol-list))))

(defun dape--inlay-hint-create-overlay ()
  "Create and prepare new overlay and maintain the old ones."
  (when-let*
      ((stack-overlay dape--stack-position-overlay)
       (buffer (overlay-buffer stack-overlay))
       (overlay
        (with-current-buffer buffer
          (pcase-let ((`(,line-start . ,line-end)
                       (save-excursion
                         (goto-char (overlay-start stack-overlay))
                         (beginning-of-line)
                         (cons (point) (line-end-position)))))
            (unless (cl-find 'dape-inlay-hint
                             (overlays-in line-start line-end)
                             :key (lambda (ov) (overlay-get ov 'category)))
              (let ((overlay (make-overlay line-start line-end)))
                (overlay-put overlay 'category 'dape-inlay-hint)
                (overlay-put overlay 'dape-symbols
                             (funcall dape--inlay-hint-symbols-fn
                                      line-start line-end))
                overlay))))))
    ;; Maintain the hints, keeping old ones based on value of
    ;; `dape-inlay-hints'.
    (setq dape--inlay-hint-overlays
          (cl-loop for overlay in (cons overlay dape--inlay-hint-overlays)
                   for index from 0
                   for max-overlays = (if (eq dape-inlay-hints t)
                                          2
                                        dape-inlay-hints)
                   if (< index max-overlays)
                   collect overlay
                   else do (delete-overlay overlay)))))

(defun dape--inlay-hint-update-overlay-contents (scopes)
  "Update overlay after-string variables in SCOPES.
This is a helper function for `dape-inlay-hints-update'."
  ;; 1. Update each overlay's symbol list with (NAME VALUE UPDATED-P)
  (cl-loop
   with all-symbols =
   (cl-loop for overlay in dape--inlay-hint-overlays
            when (overlayp overlay)
            append (overlay-get overlay 'dape-symbols))
   for scope in (reverse scopes) do
   (cl-loop for variable in (plist-get scope :variables)
            for value = (plist-get variable :value)
            for name = (plist-get variable :name) do
            (cl-loop for symbol-entry in all-symbols
                     for (hint-name previous-value) = symbol-entry
                     for updated-p = (and previous-value
                                          (not (equal previous-value value)))
                     when (equal name hint-name) do
                     (setcdr symbol-entry `(,value ,updated-p)))))
  ;; 2. Format and display the overlays after-string's
  (cl-loop
   for overlay in dape--inlay-hint-overlays
   when (overlayp overlay) do
   (cl-loop
    with symbols = (overlay-get overlay 'dape-symbols)
    for (symbol-name value updated-p) in symbols
    when value collect
    (concat
     ;; Variable name with interactive properties
     (propertize
      (format "%s :" symbol-name)
      'face 'dape-inlay-hint-face
      'mouse-face 'highlight
      'keymap
      (let ((keymap (make-sparse-keymap))
            (captured-symbol symbol-name))
        (define-key keymap [mouse-1]
                    (lambda ()
                      (interactive)
                      (dape-watch-dwim `(,captured-symbol) nil t t)))
        keymap)
      'help-echo
      (format "mouse-1: add `%s' to watch" symbol-name))
     " "
     ;; ..and value, truncating if necessary
     (propertize
      (truncate-string-to-width
       (substring value 0 (string-match-p "\n" value))
       dape-inlay-hints-variable-name-max nil nil t)
      'help-echo value
      'face (if updated-p
                'dape-inlay-hint-highlight-face
              'dape-inlay-hint-face)))
    into formatted-strings
    ;; Set after-string to display hint
    finally do
    (when formatted-strings
      (thread-last (mapconcat #'identity formatted-strings
                              dape--inlay-hint-seperator)
                   (format "  %s")
                   (overlay-put overlay 'after-string))))))

(defun dape-inlay-hints-update ()
  "Update inlay hints with current variable values."
  (when-let* (((or (eq dape-inlay-hints t)
                   (and (numberp dape-inlay-hints)
                        (< 0 dape-inlay-hints))))
              (connection (dape--live-connection 'stopped t))
              (current-frame (dape--current-stack-frame connection))
              (scopes (plist-get current-frame :scopes)))
    ;; Prepare a new overlay for current selected stack's position
    (dape--inlay-hint-create-overlay)
    ;; Fetch all scopes
    (dape--with-debounce dape--inlay-hint-debounce-timer 0.05
      (let ((responses 0))
        (dolist (scope scopes)
          (dape--with-request (dape--variables connection scope)
            (when (length= scopes (cl-incf responses))
              ;; Update each overlay with the new variables
              (dape--inlay-hint-update-overlay-contents scopes))))))))

(defun dape--inlay-hints-clean-up ()
  "Delete inlay hint overlays."
  (unless dape-active-mode
    (dolist (inlay-hint dape--inlay-hint-overlays)
      (when (overlayp inlay-hint)
        (delete-overlay inlay-hint)))
    (setq dape--inlay-hint-overlays nil)))

(add-hook 'dape-update-ui-hook #'dape-inlay-hints-update)
(add-hook 'dape-active-mode-hook #'dape--inlay-hints-clean-up)


;;; Run until point

(defun dape-until (conn)
  "Run until point.
CONN is inferred for interactive invocations."
  (interactive (list (or (dape--live-connection 'stopped t)
                         (dape--live-connection 'parent))))
  (if (cl-member 'until (dape--breakpoints-at-point)
                 :key #'dape--breakpoint-type)
      (dape-breakpoint-remove-at-point)
    (let (;; Block to ensure breakpoints changes before continue
          (dape--request-blocking t))
      ;; Disable all non disabled breakpoints temporarily
      (cl-loop for breakpoint in dape--breakpoints
               unless (or (dape--breakpoint-disabled breakpoint)
                          (eq (dape--breakpoint-type breakpoint) 'until))
               do (dape--breakpoint-disable breakpoint 'until)
               finally do (dape--breakpoints-update))
      (when-let* ((breakpoint (dape--breakpoint-place 'until)))
        ;; Bookkeeping - store until breakpoint
        (when (dape--stopped-threads conn)
          (dape-continue conn))))))

(defun dape--until-reset ()
  "Reset run until point state."
  (cl-loop with update-required-p
           for breakpoint in dape--breakpoints
           ;; Remove all `until' breakpoints
           when (eq (dape--breakpoint-type breakpoint) 'until)
           do (dape--breakpoint-remove breakpoint)
           ;; ...and re-enable disabled breakpoints
           when (eq (dape--breakpoint-disabled breakpoint) 'until)
           do
           (setq update-required-p t)
           (dape--breakpoint-disable breakpoint nil)
           finally (when update-required-p (dape--breakpoints-update))))

(add-hook 'dape-active-mode-hook #'dape--until-reset)
(add-hook 'dape-stopped-hook #'dape--until-reset)


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
       (str (string-trim
             (buffer-substring-no-properties (minibuffer-prompt-end) (point-max))))
       (`(,hint-key ,hint-config) (ignore-errors (dape--config-from-string str)))
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
                      (memq key displayed-keys)
                      (and (eq key 'port) (eq value :autoport)))
           collect key into displayed-keys and collect
           (concat
            (propertize (format "%s" key)
                        'face 'font-lock-keyword-face)
            " "
            (with-current-buffer dape--minibuffer-last-buffer
              (condition-case err
                  (propertize
                   (format "%S" (dape--config-eval-value value nil 'skip-interactive))
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
                       (mapconcat #'identity hint-rows "\n")))))
      (move-overlay dape--minibuffer-hint-overlay
                    (point-max) (point-max) (current-buffer)))))


;;; Config

(defun dape-config-get (config prop)
  "Return PROP value in CONFIG evaluated."
  (dape--config-eval-value (plist-get config prop)))

(defun dape--plistp (object)
  "Non-nil if and only if OBJECT is a valid plist."
  (and (listp object) (zerop (% (length object) 2))))

(defun dape--config-eval-value (value &optional skip-functions skip-interactive)
  "Return recursively evaluated VALUE.
If SKIP-FUNCTIONS is non nil return VALUE as is if `functionp' is
non nil.
If SKIP-INTERACTIVE is non nil return VALUE as is if `functionp' is
non nil and function uses the minibuffer."
  (pcase value
    ;; On function (or list that starts with a non keyword symbol)
    ((or (pred functionp)
         (and `(,x . ,_) (guard (and (symbolp x) (not (keywordp x))))))
     (if skip-functions
         value
       (condition-case _
           ;; Try to eval function, signal on minibuffer
           (let ((enable-recursive-minibuffers (not skip-interactive)))
             (if (functionp value)
                 (funcall-interactively value)
               (eval value t)))
         (error value))))
    ;; On plist recursively evaluate
    ((pred dape--plistp)
     (dape--config-eval-1 value skip-functions skip-interactive))
    ;; On vector evaluate each item
    ((pred vectorp)
     (cl-map 'vector
             (lambda (value)
               (dape--config-eval-value value skip-functions skip-interactive))
             value))
    ;; On symbol evaluate symbol value
    ((and (pred symbolp)
          ;; Guard against infinite recursion
          (guard (not (eq (symbol-value value) value))))
     (dape--config-eval-value (symbol-value value) skip-functions
                              skip-interactive))
    ;; Otherwise just value
    (_ value)))

(defun dape--config-eval-1 (config &optional skip-functions skip-interactive)
  "Return evaluated CONFIG.
See `dape--config-eval' for SKIP-FUNCTIONS and SKIP-INTERACTIVE."
  (cl-loop for (key value) on config by 'cddr append
           (cond
            ((memql key '(modes fn ensure)) (list key value))
            ((list key
                   (dape--config-eval-value value
                                            skip-functions
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
                                     (nreverse (seq-partition options 2))
                                     (copy-tree base-config)))))

(defun dape--config-from-string (str)
  "Return list of (KEY CONFIG) from STR.
Expects STR format:
\”ALIST-KEY KEY VALUE ... - ENV= PROGRAM ARG ...\”
Where ALIST-KEY exists in `dape-configs'."
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
      (ignore-errors
        (while
            ;; Do we have non whitespace chars after `point'?
            (thread-first (buffer-substring (point) (point-max))
                          (string-trim)
                          (string-empty-p)
                          (not))
          (let ((thing (read (current-buffer))))
            (cond
             ((eq thing '-)
              (unless (dape--plistp read-config)
                (user-error "Expecting complete options list before `-'"))
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
                                      (when args `(:args ,(apply #'vector args)))
                                      (when env `(:env ,env))))
                             read-config))
               ;; Stop and eat rest of buffer
               and return (goto-char (point-max))))
             (t
              (push thing read-config))))))
      ;; Balance half baked options list
      (when (not (dape--plistp read-config))
        (pop read-config))
      (unless (dape--plistp read-config)
        (user-error "Bad options format, see `dape-configs'"))
      (setq read-config (nreverse read-config))
      ;; Apply properties from parsed PLIST to `dape-configs' item
      (cl-loop for (key value) on base-config by 'cddr
               unless (plist-member read-config key) do
               (setq read-config (plist-put read-config key value)))
      (list name read-config))))

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

(defun dape--config-to-string (key expanded-config)
  "Create string from KEY and EXPANDED-CONFIG."
  (pcase-let* ((diff (dape--config-diff key expanded-config))
               ((map :env :program :args) expanded-config)
               (zap-form-p (and (eq dape-history-add 'shell-like)
                                (or (stringp program)
                                    (and (consp env) (keywordp (car env))
                                         (not args))))))
    (when zap-form-p
      (cl-loop for key in '(:program :env :args) do
               (setq diff (map-delete diff key))))
    (concat (when key (format "%s" key))
            (when-let* (diff (config-str (prin1-to-string diff)))
              (format " %s" (substring config-str 1 (1- (length config-str)))))
            (when zap-form-p
              (concat " -"
                      (cl-loop for (symbol value) on env by #'cddr
                               for name = (substring (symbol-name symbol) 1)
                               concat (format " %s=%s"
                                              (shell-quote-argument name)
                                              (shell-quote-argument value)))
                      (cl-loop for arg in (cons program (append args nil)) concat
                               (format " %s" (shell-quote-argument arg))))))))

(defun dape--config-ensure (config &optional signal)
  "Ensure that CONFIG is executable.
If SIGNAL is non nil raises `user-error' on failure otherwise returns
nil."
  (if-let* ((ensure-fn (plist-get config 'ensure)))
      (let ((default-directory
             (if-let* ((command-cwd (plist-get config 'command-cwd)))
                 (dape--config-eval-value command-cwd)
               default-directory)))
        (condition-case err
            (or (funcall ensure-fn config) t)
          (error
           (if signal (user-error (error-message-string err)) nil))))
    t))

(defun dape--config-mode-p (config)
  "Return non nil if CONFIG is for current major mode."
  (let ((modes (plist-get config 'modes)))
    (or (not modes)
        (apply #'provided-mode-derived-p
               major-mode (cl-map 'list 'identity modes))
        (when-let* (((not (derived-mode-p 'prog-mode)))
                    (last-hist (car dape-history))
                    (last-config
                     (cadr (ignore-errors
                             (dape--config-from-string last-hist)))))
          (cl-some (lambda (mode)
                     (memql mode (plist-get last-config 'modes)))
                   modes)))))

(defun dape--config-completion-at-point ()
  "Function for `completion-at-point' fn for `dape--read-config'."
  (let (key key-end args args-bounds last-p)
    (save-excursion
      (goto-char (minibuffer-prompt-end))
      (setq key (ignore-errors (read (current-buffer))))
      (setq key-end (point))
      (ignore-errors
        (while t
          (setq last-p (point))
          (push (read (current-buffer)) args)
          (push (cons last-p (point)) args-bounds))))
    (setq args (nreverse args)
          args-bounds (nreverse args-bounds))
    (cond
     ;; Complete key
     ((<= (point) key-end)
      (pcase-let ((`(,start . ,end)
                   (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point)))))
        (list start end
              (mapcar (lambda (suggestion) (format "%s " suggestion))
                      dape--minibuffer-suggestions))))
     ;; Complete args
     ((and (not (plist-member args '-)) ;; Skip zap/dash notation
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
  "Read configuration from minibuffer.
Completes from suggested conjurations, a configuration is suggested if
it's for current `major-mode' and it's available.
See `modes' and `ensure' in `dape-configs'."
  (let* ((suggested-configs
          (cl-loop for (key . config) in dape-configs
                   when (and (dape--config-mode-p config)
                             (dape--config-ensure config))
                   collect (dape--config-to-string key nil)))
         (initial-contents
          (or
           ;; Take `dape-command' if exist
           (when dape-command
             (dape--config-to-string (car dape-command) (cdr dape-command)))
           ;; Take first valid history item
           (seq-find (lambda (str)
                       (ignore-errors
                         (thread-first (dape--config-from-string str)
                                       (car)
                                       (dape--config-to-string nil)
                                       (member suggested-configs))))
                     dape-history)
           ;; Take first suggested config if only one exist
           (and (length= suggested-configs 1)
                (car suggested-configs))))
         (default-value
          (when initial-contents
            (pcase-let ((`(,key ,config)
                         (ignore-errors (dape--config-from-string initial-contents))))
              (list
               (dape--config-to-string
                key (ignore-errors (dape--config-eval key config)))
               (format "%s " key))))))
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
                    #'comint-filename-completion nil t)
          (add-hook 'completion-at-point-functions
                    #'dape--config-completion-at-point nil t)
          (add-hook 'after-change-functions
                    #'dape--minibuffer-hint nil t)
          (dape--minibuffer-hint))
      (pcase-let*
          ((str
            (let ((history-add-new-input (eq dape-history-add 'input)))
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
                                    (`(,key) (dape--config-from-string str)))
                                 (delete-region (minibuffer-prompt-end)
                                                (point-max))
                                 (insert (format "%s" key) " "))))
                 map)
               nil 'dape-history default-value)))
           (`(,key ,config)
            (dape--config-from-string (substring-no-properties str)))
           (evaled-config (dape--config-eval key config)))
        (unless (eq dape-history-add 'input)
          (push (dape--config-to-string key evaled-config) dape-history))
        evaled-config))))


;;; Hover

(defun dape-hover-function (cb)
  "Hook function to produce doc strings for `eldoc'.
On success calls CB with the doc string.
See `eldoc-documentation-functions', for more information."
  (when-let* ((conn (dape--live-connection 'last t))
              ((dape--capable-p conn :supportsEvaluateForHovers))
              (symbol (thing-at-point 'symbol))
              (name (substring-no-properties symbol))
              (id (plist-get (dape--current-stack-frame conn) :id)))
    (dape--with-request-bind
        (body error)
        (dape--evaluate-expression conn id name "hover")
      (unless error
        (dape--with-request
            (dape--variables-recursive conn `(:variables (,body)) '(hover)
                                       #'dape--variable-expanded-p)
          (let ((table (make-gdb-table)))
            (dape--info-scope-add-variable table (plist-put body :name name)
                                           nil '(hover) #'dape--variable-expanded-p
                                           'no-handles)
            (funcall cb (gdb-table-string table " ")))))))
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
    ["Restart" dape-restart]
    ["Quit" dape-quit]
    "--"
    ["REPL" dape-repl]
    ["Info buffers" dape-info]
    ["Memory" dape-memory
     :enable (dape--capable-p (dape--live-connection 'last)
                              :supportsReadMemoryRequest)]
    ["Disassemble" dape-disassemble
     :enable (dape--capable-p (dape--live-connection 'last)
                              :supportsDisassembleRequest)]
    "--"
    ["Customize Dape" ,(lambda () (interactive) (customize-group "dape"))]))

(defvar dape--update-mode-line-debounce-timer (timer-create)
  "Debounce context for updating the mode line.")

(defun dape--update-state (conn state &optional reason)
  "Update Dape mode line with STATE symbol for adapter CONN."
  (setf (dape--state conn) state
        (dape--state-reason conn) reason)
  (dape--with-debounce dape--update-mode-line-debounce-timer dape-ui-debounce-time
    (dape--mode-line-format)
    (force-mode-line-update t)))

(defvar dape--mode-line-format nil
  "Dape mode line format.")

(put 'dape--mode-line-format 'risky-local-variable t)

(defun dape--mode-line-format ()
  "Update variable `dape--mode-line-format' format."
  (let ((conn (or (dape--live-connection 'last t)
                  dape--connection)))
    (setq dape--mode-line-format
          `(( :propertize "dape"
              face font-lock-constant-face
              mouse-face mode-line-highlight
              help-echo "Dape: Debug Adapter Protocol for Emacs\n\
mouse-1: Display minor mode menu"
              keymap ,(let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1] dape-menu)
                        map))
            ":"
            ( :propertize
              ,(when-let* ((thread-name (plist-get (dape--current-thread conn) :name)))
                 (concat thread-name " "))
              face font-lock-constant-face
              mouse-face mode-line-highlight
              help-echo "mouse-1: Select thread"
              keymap ,(let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1] #'dape-select-thread)
                        map))
            ( :propertize ,(format "%s" (or (and conn (dape--state conn))
                                            'unknown))
              face font-lock-doc-face)
            ,@(when-let* ((reason (and conn (dape--state-reason conn))))
                `("/" (:propertize ,reason face font-lock-doc-face)))
            ,@(when-let* ((conns (dape--live-connections))
                          (nof-conns
                           (length (cl-remove-if-not #'dape--threads conns)))
                          ((> nof-conns 1)))
                `(( :propertize ,(format "(%s)" nof-conns)
                    face shadow
                    help-echo "Active child connections")))))))

(add-to-list 'global-mode-string
             `(dape-active-mode ("[" dape--mode-line-format "] ")))


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
    (define-key map "f" #'dape-restart-frame)
    (define-key map "u" #'dape-until)
    (define-key map "i" #'dape-info)
    (define-key map "R" #'dape-repl)
    (define-key map "m" #'dape-memory)
    (define-key map "M" #'dape-disassemble)
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
               dape-restart-frame
               dape-until
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
