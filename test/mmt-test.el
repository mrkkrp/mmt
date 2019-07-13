;;; mmt-test.el --- Tests for mmt (Missing macro tools for Emacs Lisp) -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–present Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/mmt
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'undercover)

(undercover "mmt.el")

(require 'mmt)
(require 'cl-lib)

;; `mmt-make-gensym-list'

(ert-deftest mmt-make-gensym-list/equality ()
  (should-not (cl-reduce #'eq (mmt-make-gensym-list 10)))
  (should-not (eq (mmt-make-gensym-list 5)
                  (mmt-make-gensym-list 5))))

;; `mmt-with-gensyms'

(defmacro mmt-with-gensyms-test (x y &rest body)
  "Construct a list using X Y without capturing anything in BODY."
  (declare (indent 2))
  (mmt-with-gensyms (a b c)
    `(let ((,a ,x)
           (,b ,y)
           (,c (+ ,x ,y)))
       ,@body
       (list ,a ,b ,c))))

(ert-deftest mmt-with-gensyms/capturing ()
  (should (equal (mmt-with-gensyms-test 2 (+ 1 2)
                   (setq a 10 b 12 c 80))
                 '(2 3 5))))

;; `mmt-with-unique-names'

(ert-deftest mmt-with-unique-names/aliasing ()
  (should (eq (symbol-function 'mmt-with-unique-names)
              'mmt-with-gensyms)))

;; `mmt-once-only'

(defmacro mmt-once-only-test (x y)
  "Evaluate X and Y once but use their values many times."
  (mmt-once-only (x y)
    `(list ,x ,y ,x ,y (+ ,x ,y))))

(defvar mmt-once-only-temp 0
  "Used in the test below to count number of evaluations.")

(ert-deftest mmt-once-only/evaluation ()
  (should (equal (mmt-once-only-test
                  (prog1 10 (cl-incf mmt-once-only-temp))
                  (prog1 20 (cl-incf mmt-once-only-temp)))
                 '(10 20 10 20 30)))
  (should (equal mmt-once-only-temp 2)))

(provide 'mmt-test)

;;; mmt-test.el ends here
