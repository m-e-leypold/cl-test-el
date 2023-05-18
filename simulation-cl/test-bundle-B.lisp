(defpackage :de.m-e-leypold.cl-test/tests-B
  (:use :common-lisp )
  (:import-from :de.m-e-leypold.cl-test :define-test)
  (:export :test-B1 :test-B2 :test-B3))

(in-package :de.m-e-leypold.cl-test/tests-B)

(define-test test-B1 ()
  "Test B1"
  t)

(define-test test-B2 ()
  "Test B2"
  nil)

(define-test test-B3 ()
  "Test B3"
  t)

