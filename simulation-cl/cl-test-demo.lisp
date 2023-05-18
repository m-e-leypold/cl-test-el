;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;; CL-TEST-EL-DEMO - Demo how to interface Common Lisp procedures with Emacs
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

(defpackage :de.m-e-leypold.cl-test
  (:use :common-lisp)
  (:export :hello :run-tests :get-test-tags)
  (:documentation 
   "This is (mostly) a mock-up of the interface cl-test will have to provide
    in order to be called from cl-test.el"))

(in-package :de.m-e-leypold.cl-test)
(named-readtables:in-readtable :interpol-syntax)

(defvar *tests* '()
  "List of tests defined with define-test"
  )

(defmacro define-test (name (&rest lambdalist) docstring &body body)
  "Define a test"
  `(progn
     (defun ,name ,lambdalist ,docstring ,@body)
     (pushnew (quote ,name) *tests*)))


;; TODO Add Domain selector.

(defun get-test-tags (selector)
  "Get all tags for all avialable test groups (e.g. :smoke, :all,
   :online, :offline, :expensive).

   SELECTOR is a data structure that tells in which suites the groups
   are going to be collected (e.g. a test suite identification or a
   regex or prefix matching the names of suites), but SELECTOR is
   ignored in this mockup.
"

  (list :smoke :all :expensive :beta))

(defun make-test-schedule (tags)
  "This is a mockup"
  (reverse *tests*))

(defun test-tagline (test)
  (documentation test 'function)) ;; should be first line, but we can mock here

(defvar *status-output* nil)
(defvar *status-rowlist* nil)

(defvar light-grey #?"\e[1;37m")
(defvar green #?"\e[1;32m")
(defvar red #?"\e[1m\e[1;31m")
(defvar blue #?"\e[0m\e[0;34m")

(defvar yellow #?"\e[33m")
(defvar normal     #?"\e[47m\e[0;30m")
(defun  cursor-position (x y) (format nil #?"\e[~a;~aH" x y))

(let ((keyword (find-package :keyword)))

  (defun insert-package-line (package)
    (let ((symbol (intern (package-name package) keyword))
	  (*print-case* :downcase))
      (format *status-output*
	      "~%~A(in-package ~A:~A~A)~A~%" light-grey blue symbol light-grey normal))))

(defun build-status-display (tests)
  (format *status-output* #?"\e[2J\e[HTest log :de.m-e-leypold.cl-test-demo~%")
  (format *status-output* #?"\e[?25l")
  ;; TODO Asses test name column width
  (let ((rowlist nil)
	(row 1)
	(package nil))
    (dolist (test tests)
      (let ((new-package (symbol-package test)))
	(if (not (equal package new-package))
	    (progn 
	      (insert-package-line new-package)
	      (setf row (+ row 2))
	      (setf package new-package))))
      (format *status-output*
	      #?" ~A⏹~A QUEUED   ~A   ~A~%"
	      light-grey
	      normal
	      (symbol-name test)
	      (test-tagline test))
      (push (list row test) rowlist)
      (setf row (1+ row)))
    (finish-output *status-output*)
    rowlist))

(defun park-cursor ()
  (let ((park-position (+ 3 (car (car *status-rowlist*)))))
    (format *status-output* (cursor-position park-position 0))))
    

(defun goto-row (test)
  (let ((row (1+ (car (find test *status-rowlist* :key #'cadr)))))
    (format *status-output* (cursor-position row 2))))


(defun log-test-running (test)
  (goto-row test)
  (format *status-output* #?"~A⏹ RUNNING~A \r" yellow normal)
  (park-cursor)
  (finish-output *status-output*))
  
(defun log-test-result (test result)
  (goto-row test)
  (if result
      (format *status-output* #?"~A⏹ PASSED ~A ~%" green normal)
      (format *status-output* #?"~A⏹ FAILED ~A ~%" red   normal)))

(defvar *failed* '())
(defvar *passed* '())

(defun log-summary ()
  (if *failed*
      (format *status-output* "~A⯈ ~A/~A tests failed ⯇~A~%"
	      red
	      (length *failed*)
	      (+ (length *failed*) (length *passed*))
	      normal)
      (format *status-output* "~AAll tests passed.~A~%"
	      green
	      normal)))

(defun run-tests (fifo tags)
  (setf *failed* '())
  (setf *passed* '())
  (with-open-file (status-output fifo :direction :output :if-exists :append)
    (let ((*status-output* status-output))
      (let ((test-schedule (make-test-schedule tags)))
	;; (let ((*status-rowlist* (build-status-display test-schedule)))
	(setf *status-rowlist* (build-status-display test-schedule))
	(dolist (test test-schedule)
	  (log-test-running test)
	  (funcall test)
	  (sleep  (/ (random 10.0) 5))
	  (let ((result
		  (funcall test)))
	    (if result
		(push test *passed*)
		(push test *failed*))
	    (log-test-result test result))
	  ;; TODO maintain stats, print final resuls
	  (park-cursor))
	(park-cursor)
	(log-summary)
	))))
    
    

(defun hello (fifo-name)
  "Just a function to test basic output to logterm via a fifo"
  (with-open-file (*status-output* fifo-name :direction :output :if-exists :append)
    (format *status-output* #?"\e[2J\e[HHello!~%")))
