# symbolic 0.0.0.9000

* Initial release.

* Added `as_symbolic()` and `is_symbolic()` for creating and identifying
  symbolic functions.

* Added `derivative()` and `integral()` for symbolic differentiation and
  integration of functions, and a `solve()` method for solving them.

* Added a `deriv()` method which mirrors `stats::deriv()`: `namevec` is a
  character vector (defaulting to every argument), and the result is a function
  (or, with `function.arg = FALSE`, an expression) whose value carries
  `"gradient"` and optionally `"hessian"` attributes. It can differentiate any
  function Maxima can, such as `asinh()`, which `stats::deriv()` can't.

* `solve()` follows `base::solve()`: its arguments keep the order `(a, b,
  target)`, and without `b` it returns the inverse function (e.g. `exp()` to
  `log()`). Results always return a matrix with one column per solution. A
  single-solution result can be manipulated further; one with several solutions
  can't.

* Symbolic algebra is performed by Maxima, via the maxima package. Results are
  rewritten into idiomatic R code (e.g. `x/y - 1` rather than
  `-1 + x * y^-1`), and Maxima functions without an R equivalent (such as
  `erf()`) are translated.

* Fractions written as literals in a function (such as `x^(1/2)`) are passed
  to Maxima exactly, rather than as floats. The same applies to `sqrt()`,
  `exp()` and `log()` of whole numbers, and to `pi`.

* `pnorm()`, `digamma()`, `trigamma()` and `psigamma()` can be used in
  symbolic functions, so results containing them can be manipulated again.

* Questions Maxima asks about arguments (such as "Is y positive, negative or
  zero?") are reported as errors, using the arguments' R names.

* `integral()` bounds are bare R expressions (e.g. `upper = 2 * y`), which may
  be infinite. Every name in a bound is a symbol, and R values are injected with
  `!!` (e.g. `lower = !!lo`). Symbols which aren't arguments of the function
  become arguments of the result, and a bound mentioning the variable of
  integration (e.g. `upper = x`) keeps it as an argument, as in a cumulative
  distribution function. Strings are not accepted.

* `target` arguments are bare names (e.g. `derivative(f, x)`), and `solve()`'s
  `b` follows the same rules as `integral()` bounds (e.g. `solve(f, 2 * y, x)`),
  so every function shares one calling convention: code is symbolic, and `!!`
  injects R values. A string naming the target still works, which allows
  injecting a name held in a variable (e.g. `derivative(f, !!var)`).

* The function argument of `as_symbolic()` is `f` (previously `.f`).

* Names in the body of a symbolic function which aren't arguments are
  evaluated as ordinary R code, and an undefined name gives an error suggesting
  it be made an argument. `integral(f, 0, 1)` hints that `target` comes before
  the bounds.

* `integral()` and `solve()` error when Maxima can't find a closed form or an
  explicit solution, rather than returning an unevaluated result.
