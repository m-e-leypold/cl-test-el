;;; ------------------------- -*- mode: emacs-lisp; lexical-binding: t -*-
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


;;; Creating a log/status terminal buffer --------------------------------------

(defvar logterm-path (file-name-directory load-file-name)
  "Were the loaded copy of logterm.el lives")
(setf logterm-path (file-name-directory load-file-name))

(defun logterm-create (&optional buffer-name)
  (if (not buffer-name)
      (setf buffer-name "logterm"))
  ;; TODO slime bindings in this buffer (how?)
  ;; TODO: Find existing buffer, kill it or recycle
  ;; TODO: Set  buffer local variable to fifo
  ;; TODO: Loop parameter for reusable buffers,
  ;;       otherwise terminate when fifo closed
  ;;       And then re-use means new buffer. 
  (ansi-term (expand-file-name "el-logterm-reader" logterm-path) buffer-name)
  (let ((status-fifo nil))
    (while (not status-fifo)      
      (let ((start (point-min)))
	(goto-char start)
	(let* ((first-line
		(buffer-substring-no-properties start (line-end-position)))
	       (len (length first-line)))
	  (if (and (< 0 len) (equal ?\) (aref first-line (1- len))))		
	      (progn
		(let ((start (string-search "(" first-line)))
		  (setf status-fifo
			(substring first-line (1+ start) (1- len)) start len)))
	    (sleep-for 0.2)))))
	  (cl-values status-fifo (current-buffer))))


;;; * provides -----------------------------------------------------------------

(provide 'logterm)
