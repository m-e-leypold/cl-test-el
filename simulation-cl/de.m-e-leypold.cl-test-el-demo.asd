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


(defsystem "de.m-e-leypold.cl-test-el-demo"
    :description "Demo how to interface cl-test with emacs"
    :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
    :licence "GPL3"
    :depends-on ("cl-interpol")
    :components ((:file "cl-test-demo")))

(defsystem "de.m-e-leypold.cl-test-el-demo/tests"
    :description "Mocked up tests for cl-test-el-demo"
    :author "M E Leypold [elegant-weapons (AT) m-e-leypold (DOT) de]"
    :licence "GPL3"
    :depends-on ("cl-interpol" "de.m-e-leypold.cl-test-el-demo")
    :components ((:file "test-bundle-A")
		 (:file "test-bundle-B")))
