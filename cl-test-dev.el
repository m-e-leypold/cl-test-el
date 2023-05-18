;;; --------------------- -*- mode: emacs-lisp; lexical-binding: t -*-
;;;
;;; cl-test-el - An emacs interface to cl-test
;;; Copyright (C) 2023  M E Leypold
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

;;; * Requires -----------------------------------------------------------------

(require 'cl-test-utils)
(require 'cl-lib)

;;; * Installation -------------------------------------------------------------

(defvar cl-test-dev-source-path "/home/mel/my/cl-test-el"
    "The path where the cl-test-el package source resides")

(defun cl-test-remove* ()
  (let ((spec (cadr (assoc 'cl-test (package--alist)))))
    (if spec
	(package-delete spec)
      nil)))

(defun cl-test-remove ()
  (interactive)
  (while (cl-test-remove*) ))
    
(defun cl-test-install-lisp-only ()
    (interactive)
    (cl-test-remove)
    (if (not (file-exists-p (expand-file-name "VERSION" cl-test-dev-source-path)))
	(with-temp-buffer
	    (cd cl-test-dev-source-path)
	    (cl-multiple-value-bind (exit-code output)
		(cl-test-run-external-program "make" "cl-test-pkg.el")
		(if (not (= 0 exit-code))
		    (progn
		      (message "errors running make =>\n%s" output)
		      (error "Running make to get/update work-pkg.el failed"))))))
    (package-install-file cl-test-dev-source-path)
    (cl-test-reload-installed))

(defun cl-test-install ()
  (interactive)
  (with-temp-buffer
    (cd cl-test-dev-source-path)
    (cl-multiple-value-bind (exit-code output)
	(cl-test-run-external-program "make" "package")
      (if (not (= 0 exit-code))
	  (progn
	    (message "errors running make =>\n%s" output)
	    (error "Running make to get/update work-pkg.el failed"))))
    (cl-test-remove)
    (package-install-file ".build/PACKAGE.tar"))
  (cl-test-reload-installed))

;;; * Loading ------------------------------------------------------------------

(defun cl-test-make-symbol-in-package-p (package-symbol)
  (let* ((package-name (symbol-name package-symbol))
	 (package-name-length (length package-name))
	 (prefix-length (1+ package-name-length)))
    #'(lambda (symbol)
	(let* ((name (symbol-name symbol))
	       (name-length (length name)))
	  (or
	   (equal name package-name)
	   (and
	    (<= prefix-length name-length)
	    (equal package-name (substring name 0 package-name-length))
	    (cl-find (aref name package-name-length) "-/.,")))))))

(defun cl-test-forget-features ()
  ;; Also need to forget logterm and swank-call :-(
  (setf features
	(cl-remove-if
	 (cl-test-make-symbol-in-package-p 'swank-call)
	 (cl-remove-if
	  (cl-test-make-symbol-in-package-p 'logterm)
	  (cl-remove-if
	   (cl-test-make-symbol-in-package-p 'cl-test)
	   features)))))

(defun cl-test-reload-installed ()
  (interactive)
  (cl-test-forced-reload))

(defun cl-test-forced-reload ()
  (cl-test-forget-features)
  (require 'cl-test)
  (require 'cl-test-dev))

(defun cl-test-reload ()
  (interactive)
  (let ((force-load-messages t))
    (cl-test-forced-reload)))


(defun cl-test-switch-to-source (use-this-path-p)
  (interactive "P")
  (if use-this-path-p
      (setf cl-test-dev-source-path default-directory))
  (push cl-test-dev-source-path load-path)
  (cl-test-reload))

;;; * Development --------------------------------------------------------------


;;; * Keybinding ---------------------------------------------------------------

(defun cl-test-load-interactive (prefix) 
  (interactive "P")
  (if prefix
      (progn
	(message "%s" "Reloading cl-test ...")
	(cl-test-reload)
	(message "%s" "cl-test reloaded."))
    (progn
      (message "%s" "Reloading buffer ...")
      (load-file buffer-file-name)
      (message "Buffer reloaded: %s" (current-buffer)))))

(define-key emacs-lisp-mode-map (kbd "C-c l")   'cl-test-load-interactive)
(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'cl-test-load-interactive)

;;; * Provides -----------------------------------------------------------------

(provide 'cl-test-dev)
