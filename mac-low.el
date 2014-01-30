;;; -*- coding: utf-8; lexical-binding: t -*-
;;; mac-low.el --- Providing some low level macros
;; 
;; Filename: eparse-base.el
;; Description: Some macros to use write emacs lisp and day use.
;; Author: derui
;; Maintainer: derui
;; Copyright (C) 2013, derui, all rights reserved.
;; Created: Nov 13 2013
;; Version: 0.0.0
;; Keywords: macro
;; Compatibility: 24.3 <=
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; mac-low provide some macros that are low level macros and useful for
;; day use.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 0.0.0 : Initial
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'dash)
(require 'cl-lib)

(defmacro maclow:aif (it &rest body)
  "Anafolic if"
  `(let ((it ,it))
     (if it
         ,@body)
     ))
(put 'maclow:aif 'lisp-indent-function 2)

(defmacro maclow:awhen (it &rest body)
  "Anafolic when"
  `(let ((it ,it))
     (when it
       ,@body)))
(put 'maclow:awhen 'lisp-indent-function 1)

(defun maclow:%g!-symbol-p (sym)
  "Return the symbol given as argument is g! prefixed symbol or not"
  (and (symbolp sym)
       (< 2 (length (symbol-name sym)))
       (string= "g!" (substring (symbol-name sym) 0 2))))

(defun maclow:%o!-symbol-p (sym)
  "Return the symbol given as argument is o! prefixed symbol or not"
  (and (symbolp sym)
       (< 2 (length (symbol-name sym)))
       (string= "o!" (substring (symbol-name sym) 0 2))))

(defmacro maclow:%defmacro!g (name args &rest body)
  "Define new macro that do gensym each symbols in the body only g! prefix symbols"

  ;; make interned symbols each symbols prefixed g!
  (let ((syms (cl-loop for sym in (cl-remove-duplicates
                                   (cl-remove-if-not #'maclow:%g!-symbol-p
                                                     (-flatten body)))
                       return (intern (symbol-name sym))))
    `(defmacro ,name ,args
       (let ,(cl-mapcar #'(lambda (s)
                            `(,s ,(make-symbol (substring (symbol-name s) 2))))
                        syms)
         ,@body
        ))))
(put 'maclow:%defmacro!g 'lisp-indent-function 2)



(provide 'mac-low)
