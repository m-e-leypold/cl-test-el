;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;; CL-TEST-SLIME - Demo how to interface Common Lisp procedures with Emacs
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

(defpackage :de.m-e-leypold.cl-test-slime
  (:use :common-lisp)
  (:export :hello :get-test-sets :test-run :demo-status-output))

(in-package :de.m-e-leypold.cl-test-slime)
(named-readtables:in-readtable :interpol-syntax)

(defun hello (&optional name)
  (if name
      (format nil "Hello, ~a!" name)
      "Hello!"))

(defun get-test-sets ()
  '(:all :smoke :units :integration :beta))

(defvar *log-output* nil)
(defvar *report-output* nil)
(defvar *status-output* nil)

(defun test-run (test-sets)
  (let ((*log-output* (swank-buffer-streams:make-buffer-output-stream :test-log)))
    (format *log-output* "Tests sets ~A~%" test-sets)
    (finish-output *log-output*)
    (close *log-output*)))

(defun print-package (symbol)
  (format *status-output* #?"\e[0;37m(in-package \e[0;34m~S\e[0;37m)\e[0;30m~%" symbol))

(defun print-running (symbol text)
  (format *status-output* #?" \e[1;37m⏹\e[1;47m\e[0;30m \e[1;33mRUNNING\e[0;30m  ~A  ~A" (symbol-name symbol) text)
  (finish-output *status-output*))

(defun print-passed (symbol)
  (format *status-output* #?"\r \e[1;32m⏹\e[1;47m\e[0;30m \e[1;32mPASSED\e[0;30m   ~A~%" (symbol-name symbol))
  (finish-output *status-output*))
(defun print-failed (symbol)
  (format *status-output* #?"\r \e[1;31m⏹\e[1;47m\e[0;30m \e[1;31mFAILED\e[0;30m   ~A~%" (symbol-name symbol))
  (finish-output *status-output*))

;; TODO: Add "RUNNING" and overwriting to simulation
;; TODO: Add colored summary "OK. <n> tests passed" or "FAILURES: <n>/<k> tests."

(defun test-1 ()  
  )

(defun test-2 ()
  )

;; TODO: Try to print list before with (grey) QUEUED as status

(defun demo-status-output (fifo-name)
  (format t "demo-status-output")
  (with-open-file (*status-output* fifo-name :direction :output :if-exists :append)

    (format *status-output* #?"\e[2J\e[HTest log \e[1m:de.m-e-leypold.cl-test-slime\e[0m~%~%")
    
    (print-package :de.m-e-leypold.cl-test-slime)    
    (print-running 'test-1 "Nam a sapien.")
    (sleep 2)
    (print-passed 'test-1)

    (print-running 'test-2 "Praesent fermentum tempor tellus.")
    (sleep 2)
    (print-failed 'test-2)
    
    (print-package :de.m-e-leypold.foobar)
    (print-running 'test-4 "Donec pretium posuere tellus.")
    (sleep 1)
    (print-failed 'test-4 )
    (print-running 'test-3 "Pellentesque dapibus suscipit ligula.")
    (sleep 1)
    (print-passed 'test-3)

    (print-running 'test-1 "Aenean in sem ac leo mollis blandit.")
    (sleep 0.5)
    (print-passed 'test-1)

    (print-running 'test-2 "Donec pretium posuere tellus.")
    (sleep 0.5)
    (print-failed 'test-2)

    
    (finish-output *status-output*)))


