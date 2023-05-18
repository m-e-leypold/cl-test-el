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

;;; * Dependecies -------------------------------------------------------------|

(require 'logterm)
(require 'swank-call)

;;; * Variables ---------------------------------------------------------------|

(defvar cl-test-path (file-name-directory load-file-name)
  "Were the loaded copy of cl-test lives")
(setf cl-test-path (file-name-directory load-file-name))

(message "load-file-name => %S\n" load-file-name)
(message "cl-test-path   => %S\n" cl-test-path)


;;; * Common functions --------------------------------------------------------|

(defvar cl-test-version nil)

(defun cl-test-version* ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "cl-test-pkg.el" cl-test-path))
    (setf cl-test-version (caddr (read (current-buffer))))
    cl-test-version))

;;;###autoload
(defun cl-test-version ()
  (interactive)
  (message "cl-test-version => %s" (list (cl-test-version*) cl-test-path)))

;;; * Interfacing cl-test (at the CL side) ------------------------------------|

(defvar cl-test-selected-suites :*
  "The suites to be offered in the menu. Typically this variable
   would be set by a test.el or test-config.el in the lisp
   package, or explictely by some other config file the user has
   to load.

   This variable exists to set the general scope of the tests and
   be able to have multiple suites for multiple packages
   simultaneously loaded at the slime / common lisp side without
   necessary having to load them all.

   :* means to select all available suites.

   Currently (in the simulation / mock-up) this selector is
   ignored.")

(defun cl-test-get-test-tags ()
  "Obtain all available test group names (tags) from
   the common lisp side."
  
  (swank-call/funcall
   'de.m-e-leypold.cl-test:get-test-tags cl-test-selected-suites))

(defvar cl-test-available-tags '()
  "The available test groups (their names).

   Will be set by cl-test-run as a side effect.")

(defvar cl-test-recent-tags ''()
  "The recently selected test groups (their names).

   Will be set by cl-test-run as a side effect.")

(setf cl-test-recent-tags nil)  ;; reset to NIL when reloading.

(defun cl-test-query-tags (available-tags default-tags) 
  "Query the user for the test groups that should be executed."  
  (cl-test-keywords-from-strings
   (completing-read-multiple
    (format "test sets? default = %S > "
	    (cl-test-strings-from-keywords default-tags))
    (cl-test-strings-from-keywords available-tags))))

(defun cl-test-run (choose-tags-p)
  "Run the tests (on the CL side). Select the test groups if none
   were selected before. With a prefix argument CHOOSE-TAGS-P
   always select the groups."
  
  (interactive "P")
  (let ((tags (cl-test-get-test-tags)))
    (setf cl-test-available-tags tags)
    (if (not cl-test-recent-tags)
	(setf cl-test-recent-tags (list (car tags))))
    (if choose-tags-p
	(setf cl-test-recent-tags
	      (cl-test-query-tags tags cl-test-recent-tags)))    
    )
  (cl-multiple-value-bind (fifo buffer)
      (logterm-create)
    (swank-call/funcall-async
     'de.m-e-leypold.cl-test:run-tests fifo cl-test-recent-tags)))

;;; * Interactive demos and tests ---------------------------------------------|

(defun cl-test-prepare-demo ()
  (if (or (not (boundp 'slime-default-connection))
	  (not slime-default-connection))
      (progn
	(slime)   
	(while (not (swank-call/ping))
	  (message "cl-test: Slime not ready waiting")
	  (sleep-for 0.2))))
  (swank-call/funcall 'cl:load "load-simulation.lisp")
  (delete-other-windows)
  (split-window-right)
  (other-window 1))

;;;###autoload
(defun cl-test-hello ()

  "Test output to logterm buffer"
  
  (interactive)

  ;; ATM slime must be running
  ;; TODO Start slime if not already running.
  ;; TODO Load simulation
  
  (cl-multiple-value-bind (fifo buffer)
      (logterm-create)
    (swank-call/funcall-async 'de.m-e-leypold.cl-test:hello fifo)))

;;;###autoload
(defun cl-test-demo (choose-tags-p)

  "Simulate a test run."

  (message "--- cl-test demo starting ---")
  
  (cl-test-prepare-demo)
  
  (interactive "P")
  (cl-test-run choose-tags-p)

  (message "--- cl-test demo done ---"))

;;;###autoload
(defun cl-test-dev (use-this-path-p)
  (interactive "P")
  (require 'cl-test-dev)
  (cl-test-switch-to-source use-this-path-p)
  (find-file (expand-file-name "." cl-test-dev-source-path)))

;;; * provides ----------------------------------------------------------------|

(cl-test-version)

(provide 'cl-test)

