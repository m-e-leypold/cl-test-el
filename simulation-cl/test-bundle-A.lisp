(defpackage :de.m-e-leypold.cl-test/tests-A
  (:use :common-lisp )
  (:import-from :de.m-e-leypold.cl-test :define-test)
  (:export :test-A1 :test-A2 :test-A3))

(in-package :de.m-e-leypold.cl-test/tests-A)

(define-test test-A1 ()
  "Some test, a short description"
  t)

(define-test test-A2 ()
  "Another test that will test s.th. else"
  t)

(define-test test-A3 ()
  "A third test in the series"
  nil)
