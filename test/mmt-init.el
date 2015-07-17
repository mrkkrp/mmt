;;; mmt-init.el --- Tests for mmt (Missing macro tools for Emacs Lisp) -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
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

;; `mmt-gensym'

(ert-deftest mmt-gensym/equaluality ()
  (should-not (eq (mmt-gensym) (mmt-gensym)))
  (should-not (eq (mmt-gensym "A") (mmt-gensym "A")))
  (should-not (eq (mmt-gensym 5) (mmt-gensym 5)))
  (should (let ((x (mmt-gensym))) (eq x x))))

;; `mmt-make-gensym-list'

;; `mmt-with-gensyms'

;; `mmt-once-only'

(require 'ert)

(provide 'mmt-init)

;;; mmt-init.el ends here
