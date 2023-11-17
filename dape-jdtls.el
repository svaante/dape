;;; package --- Integration of Dape & JDTLS  -*- lexical-binding: t -*-

;; Authors: Magiel Bruntink and Daniel Pettersson
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: 
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

;; Requirements:

;; JDK version 17+, installed with JAVA_HOME set and binaries on PATH.

;; Eclipse JDT Language Server, installed with binaries on PATH.
;; Get from https://github.com/eclipse-jdtls/eclipse.jdt.ls
;; (MacOs) $ brew install jdtls

;; Java Debug Server for Visual Studio Code
;; Customize 'dape-jdtls-java-debug-plugin-jar' to point to the plugin jar.
;; Get from https://github.com/microsoft/java-debug

;; Options for the Java Debug Server are documented here:
;; https://github.com/microsoft/vscode-java-debug#options

;; VS Code Test Runner for Java (here for reference, not yet needed)
;; Customize 'dape-jdtls-java-test-plugin-jar' to point to the plugin jar.
;; Get from https://github.com/microsoft/vscode-java-test

;; Support:

;; At the moment only "launch" configurations are supported - which
;; allows a user to debug a program through a main class.

;; Not supported;

;; - Attaching a debugger to a running Java application ("attach").
;; - Running or debugging specific (at point) unit tests.

;;; Code:

(require 'dape)
(require 'eglot)

(defcustom dape-jdtls-java-debug-plugin-jar
  "/PATH/TO/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-VERSION.jar"
  "The path to the Java Debug Server plugin jar, see https://github.com/microsoft/java-debug"
  :type '(string)
  :group 'dape-jdtls)

(defcustom dape-jdtls-java-test-plugin-jar
  "/PATH/TO/vscode-java-test/java-extension/com.microsoft.java.test.plugin/target/com.microsoft.java.test.plugin-VERSION.jar"
  "The path to the VS Code Java Test plugin jar, see https://github.com/microsoft/vscode-java-test"
  :type '(string)
  :group 'dape-jdtls)


(defun dape-jdtls-complete-config (config)
  (let ((program (dape-jdtls--complete-program config)))
    (with-current-buffer (find-file program)
      (let ((server (eglot-current-server)))
	(if (not server)
	    (user-error "Abort, no Eglot LSP server appears to be active for buffer %s" (buffer-name))
	  (let* ((entrypoint (dape-jdtls--complete-entrypoint server config)))
	    (seq-uniq
	     (append config
		     (list :projectName (plist-get entrypoint :projectName)
			   :mainClass (plist-get entrypoint :mainClass)
			   :program program
			   :classPaths (dape-jdtls--complete-classpath server entrypoint config)
			   :request "launch"
			   :type "java"
			   :console "internalConsole"
			   'port (dape-jdtls--complete-port server))))))))))

(defun dape-jdtls--complete-program (config)
  (let ((program (plist-get config :program)))
    (if program program
      (dape-find-file-buffer-default))))

(defun dape-jdtls--complete-entrypoint (server config)
  (let ((project (plist-get config :projectName))
	(main-class (plist-get config :mainClass)))
    (if (and project main-class)
	(list :projectName project :mainClass main-class)
      (let ((entrypoints (eglot-execute-command server "vscode.java.resolveMainClass"
						(if project project
						  (project-name (project-current))))))
	(when (length= entrypoints 0)
	  (error "Abort, no main classes found in project %s" (eglot-project-nickname server)))
	(dape-jdtls--select-entrypoint entrypoints config)))))

(defun dape-jdtls--select-entrypoint (entrypoints config)
  (let* ((separator "/")
	 (candidates (mapcar (lambda (entrypoint)
			       (s-concat (plist-get entrypoint :projectName) separator
                                         (plist-get entrypoint :mainClass)))
			     (append entrypoints '())))
	 (user-input (s-concat (plist-get config :projectName) separator (plist-get config :mainClass)))
	 (selected (completing-read "Select entrypoint: " candidates nil t
				    (unless (s-equals? user-input separator) user-input)))
	 (selected-split (s-split separator selected)))
    (list :projectName (nth 0 selected-split) :mainClass (nth 1 selected-split))))

(defun dape-jdtls--complete-classpath (server entrypoint config)
  (elt (eglot-execute-command server "vscode.java.resolveClasspath"
			      (vector (plist-get entrypoint :mainClass)
				      (plist-get entrypoint :projectName)))
       1))

(defun dape-jdtls--complete-port (server)
  (eglot-execute-command server "vscode.java.startDebugSession" nil))

(provide 'dape-jdtls)
