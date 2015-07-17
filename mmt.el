;;; mmt.el --- Missing macro tools for Emacs Lisp -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/mmt
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.3"))
;; Keywords: macro, emacs-lisp
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

;; The package contains classic tools for Emacs Lisp developers who want to
;; write macros with convenience.
;;
;; The following functions and macros are present:
;;
;; * mmt-gensym
;; * mmt-with-gensyms
;; * mmt-with-unique-names

;;; Code:

(defvar mmt--gensym-counter 0
  "This is the counter that `mmt-gensym' uses.")

(defun mmt-gensym (&optional x)
  "Create and return new uninterned symbol as if by calling `make-symbol'.

The only difference between `mmt-gensym' and `make-symbol' is in
how the new symbol's name is determined.  The name is
concatenation of a prefix which defaults to \"G\" and a suffix
which is decimal representation of a number that defaults to the
value of `mmt--gensym-counter'.

If X is supplied and is a string, then that string is used as a
prefix instead of \"G\" for this call to `mmt-gensym' only.

If X is supplied and is an integer, then that integer is used
instead of the value of `mmt--gensym-counter' as the suffix for
this call of `mmt-gensym' only.

If and only if no explicit suffix is supplied
`mmt--gensym-counter' is incremented after it is used."
  (let ((prefix (if (stringp x) x "G"))
        (suffix (if (integerp x)
                    x
                  (prog1 mmt--gensym-counter
                    (setq mmt--gensym-counter
                          (1+ mmt--gensym-counter))))))
    (make-symbol (format "%s%d" prefix suffix))))

(defmacro mmt-with-gensyms (names &rest body)
  "Bind each variable in NAMES to a unique symbol and evaluate BODY.

Each of NAMES must be either a symbol, or of the form:

  (SYMBOL STRING-OR-SYMBOL)

Bare symbols appearing in NAMES are equivalent to:

  (SYMBOL SYMBOL)

The STRING-OR-SYMBOL is used as the argument to `mmt-gensym' when
constructing the unique symbol the named variable will be bound
to."
  `(let ,(mapcar (lambda (name)
                   (cl-destructuring-bind (symbol . prefix)
                       (if (consp name)
                           (cons (car name) (cadr name))
                         (cons name name))
                     (list symbol
                           (mmt-gensym
                            (if (symbolp prefix)
                                (symbol-name prefix)
                              prefix)))))
                 names)
     ,@body))

(defalias 'mmt-with-unique-names 'mmt-with-gensyms)

(provide 'mmt)

;;; mmt.el ends here
