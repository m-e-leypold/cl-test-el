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

(push 'slime-buffer-streams slime-contribs)
(require 'slime-autoloads)

;;; * Synchronous calls --------------------------------------------------------

(defun swank-call/eval (form)
  (message "swank-call/eval => %S" form)
  (read
   (cadr (slime-eval
	  `(swank:eval-and-grab-output
	    ,(format "%S" form))))))


(defun swank-call/ping ()
  (condition-case nil
      (swank-call/eval t)
    (error nil)))

(defun swank-call/apply (symbol &rest args)
  (swank-call/eval (cons symbol
			 (mapcar #'(lambda (x) `(quote ,x))
				 (apply #'list args)))))


(defun swank-call/funcall (symbol &rest args)
  (swank-call/eval
   (cons symbol
	 (mapcar #'(lambda (x) `(quote ,x))
		 args))))

;;; * Async calls --------------------------------------------------------------

(defun swank-call/eval-async (form) ;; TODO add continuation
  (message "swank-call/eval-async => %S" form)
  (slime-eval-async
      `(swank:eval-and-grab-output
	,(format "%S" form))))


(defun swank-call/funcall-async (symbol &rest args)
  (swank-call/eval-async
   (cons symbol
	 (mapcar #'(lambda (x) `(quote ,x))
		 args))))


;;; * provides ----------------------------------------------------------------|

(provide 'swank-call)
