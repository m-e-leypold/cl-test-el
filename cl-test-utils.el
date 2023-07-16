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

;;; * Requires -----------------------------------------------------------------

(require 'cl-lib)

;;; * Running external programs ------------------------------------------------

(defun cl-test-run-external-program (program &rest args)
    "Run PROGRAM with ARGS and return the exit code and output in a list."
    (with-temp-buffer 
	(cl-values
	    (apply 'call-process program nil (current-buffer) nil args)
            (buffer-string))))

;;; * Path names ---------------------------------------------------------------

(defun cl-test-make-absolute-path (path &optional default-directory)
  (if (not default-directory)
      (setf default-directory (getenv "HOME")))  
  (if (file-name-absolute-p path)
      path
    (expand-file-name path default-directory)))
  
(defun cl-test-probe-paths (paths indicator-file &optional default-directory)
  (if (not default-directory)
      (setf default-directory (getenv "HOME")))
  (let ((found nil))
    (while (and (not found) paths)
      (let ((path
	     (let ((p (car paths)))
	       (if (file-name-absolute-p p)
		   p
		 (expand-file-name p default-directory)))))
	(setf paths (cdr paths))
	(if (file-exists-p (expand-file-name indicator-file path))
	    (setf found path))))
    found))


;;; * Keywords -----------------------------------------------------------------

(defun cl-test-strings-from-keywords (keywords)
  (cons "ALL"
	(mapcar #'(lambda (s)		    
		    (downcase (substring (symbol-name s) 1)))
		keywords)))

(defun cl-test-keywords-from-strings (names)
  (mapcar #'(lambda (s)
	      (if (string-equal s "ALL")
		  'T
		(intern
		 (concat ":" s))))
	  names))


;;; * Provides -----------------------------------------------------------------

(provide 'cl-test-utils)


