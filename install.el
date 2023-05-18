;; -------------------------------- -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; cl-test-el - An emacs interface to cl-test
;; Copyright (C) 2023  M E Leypold
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; -----------------------------------------------------------------------------
;;
;; This emacs lisp script is execute with 'emacs --batch' by 'make install'
;; to install the lates built package.

(push default-directory load-path)
(require 'package)
(package-initialize)
(require 'org-roam)
(load-file "cl-test-dev.el")
(cl-test-remove)
(package-install-file ".build/PACKAGE.tar")
