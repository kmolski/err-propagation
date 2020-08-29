# err-propagation

A collection of functions for differentiation and propagation of error
on formulas with arbitrary variables.

So far, it has only been tested with GNU Guile Scheme 2.2.6.

Credits:
-------

The derivation procedure used in this program is adapted from the book
"Structure and Interpretation of Computer Programs, Second Edition" by H. Abelson, G.J. Sussman and J. Sussman.

Usage:
------

To start the Guile REPL with the functions loaded, run:

```sh
guile -l err-propagation.scm
```

Examples:
--------

Propagation of error for formula `2 * f * dx-avg`:

```scheme
scheme@(guile-user)> (err-propagate '(* 2 (* f dx-avg)) '((dx-avg 0.1) (f 0.01)))
$1 = (sqrt (+ (^ (* (* 2 dx-avg) 0.01) 2) (^ (* (* 2 f) 0.1) 2)))
```

The result of `err-propagate` can also be evaluated:

```scheme
scheme@(guile-user)> (define dx-avg 0.072)
scheme@(guile-user)> (define f 1200)
scheme@(guile-user)> (eval $1 (interaction-environment))
$2 = 240.00000000432
```

or turned into an Excel-compatible infix formula:

``` scheme
scheme@(guile-user)> (expr-to-infix-str $1)
$3 = "SQRT(((2 * dx-avg) * 0.01) ^ 2 + ((2 * f) * 0.1) ^ 2)"
```

License:
--------

[GNU General Public License version 3](https://opensource.org/licenses/GPL-3.0)
