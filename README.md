# Missing macro tools for Emacs Lisp

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/mmt-badge.svg)](https://melpa.org/#/mmt)
![CI](https://github.com/mrkkrp/mmt/workflows/CI/badge.svg?branch=master)

The following functions and macros are present:

* `mmt-make-gensym-list`
* `mmt-with-gensyms`
* `mmt-with-unique-names`
* `mmt-once-only`

## Installation

The package is available via MELPA, so you can just type `M-x
package-install RET mmt RET`.

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`. Then you can require it in your init file like
this:

```emacs-lisp
(require 'mmt)
```

Don't forget to include it in your list of dependencies if you are writing
an Emacs Lisp package:

```emacs-lisp
;; Package-Requires: ((emacs "24.5") (mmt "0.1.1"))
```

## API

`cl-gensym` is provided by `cl-lib`, which ships with Emacs.

```
mmt-make-gensym-list length &optional x
```

Return a list of `length` gensyms.

Each element of the list is generated as if with a call to `mmt-gensym`
using the second argument `x` (defaulting to `"G"`).

----

```
mmt-with-gensyms names &rest body
```

Bind each variable in `names` to a unique symbol and evaluate `body`.

Each element of `names` must be either a symbol, or of the form:

```
(symbol string-or-symbol)
```

Bare symbols appearing in `names` are equivalent to:

```
(symbol symbol)
```

The `string-or-symbol` is used (converted to a string if necessary) as the
argument to `mmt-gensym` when constructing the unique symbol the named
variable will be bound to.

----

```
mmt-with-unique-names names &rest body
```

This is an alias for `mmt-with-gensyms`.

----

```
mmt-once-only specs &rest body
```

Rebind symbols according to `specs` and evaluate `body`.

Each element of `specs` must be either a symbol naming the variable to be
rebound or of the form:

```
(symbol initform)
```

where `initform` is guaranteed to be evaluated only once.

Bare symbols in `specs` are equivalent to

```
(symbol symbol)
```

Example:

```emacs-lisp
(defmacro cons1 (x)
  (mmt-once-only (x) `(cons ,x ,x)))

(let ((y 0))
  (cons1 (incf y))) ;; ⇒ (1 . 1)
```

## License

Copyright © 2015–present Mark Karpov

Distributed under GNU GPL, version 3.
