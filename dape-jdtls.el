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
;;; 

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

(defun dape-jdtls-config-fn (config)
  (with-current-buffer (find-file (plist-get config :program))
    (let ((server (eglot-current-server)))
      (if server
	  (let* ((entrypoints (dape-jdtls--get-entrypoints server))
		 (selected-entrypoint (dape-jdtls-select-entry-point entrypoints config))
		 (classpaths (dape-jdtls--get-classpaths server selected-entrypoint))
		 (debug-port (dape-jdtls--get-debug-port server)))
	    (append (list
		     'port debug-port
		     :mainClass (plist-get selected-entrypoint :mainClass)
		     :projectName (plist-get selected-entrypoint :projectName)
		     :classPaths classpaths
		     :type "java"
		     :console "dape"
		     :request "launch")
		    config))
	(user-error "Abort, no Eglot LSP server appears to be active for buffer %s" (buffer-name))))))

(defun dape-jdtls--get-entrypoints (server)
  (let ((entrypoints (eglot-execute-command server "vscode.java.resolveMainClass"
					    (project-name (project-current)))))
    (when (length= entrypoints 0)
      (error "Abort, no main classes found in project %s" (eglot-project-nickname server)))
    entrypoints))

(defun dape-jdtls--get-classpaths (server entrypoint)
  (elt (eglot-execute-command server "vscode.java.resolveClasspath"
			      (vector (plist-get entrypoint :mainClass)
				      (plist-get entrypoint :projectName)))
       1))

(defun dape-jdtls--get-debug-port (server)
  (eglot-execute-command server "vscode.java.startDebugSession" nil))

(defun dape-jdtls-select-entry-point (entry-points config)
  (let* ((separator "/")
	 (candidates (mapcar (lambda (entry-point)
			       (s-concat (plist-get entry-point :projectName) separator
                                         (plist-get entry-point :mainClass)))
			     (append entry-points'())))
	 (user-input (s-concat (plist-get config :projectName) separator (plist-get config :mainClass)))
	 (selected (completing-read "Select entrypoint: " candidates nil t
				    (unless (s-equals? user-input separator) user-input)))
	 (selected-split (s-split separator selected)))
    (list :projectName (nth 0 selected-split)
	  :mainClass (nth 1 selected-split))))

(provide 'dape-jdtls)
