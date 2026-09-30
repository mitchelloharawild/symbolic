#' Symbolic functions
#'
#' @description
#' `as_symbolic()` marks a function as symbolic, allowing it to be manipulated
#' algebraically by [derivative()], [integral()] and [solve()].
#'
#' Symbolic functions are ordinary R functions and can be evaluated as usual.
#' The class only affects how the function is printed, and which methods are
#' used to transform it.
#'
#' @param f A function to treat symbolically. Its body must consist of
#'   operations understood by [Maxima](https://maxima.sourceforge.io/) (via
#'   the maxima package), and its formals must not include `...`.
#' @param x An object.
#' @param ... Not used.
#'
#' @return
#' `as_symbolic()` returns `f` with the class `"symbolic"` added.
#'
#' `is_symbolic()` returns `TRUE` if `x` is a symbolic function, and `FALSE`
#' otherwise.
#'
#' @section How functions are converted:
#' To transform a symbolic function, its body is evaluated with each argument
#' replaced by a Maxima symbol. Default values of arguments are ignored, so
#' every argument is a free symbol.
#'
#' Names in the body which aren't arguments follow R's usual rules for
#' closures: they are evaluated as ordinary R code, in the function's
#' environment, and their current values are fixed into the result.
#'
#' ```
#' a <- 2
#' derivative(as_symbolic(function(x) a * x^2))
#' #> {x} -> 4 * x
#' ```
#'
#' A name which isn't defined is an error. To keep a name symbolic, make it an
#' argument of the function, as in `function(x, a) a * x^2`.
#'
#' This differs from the other arguments of [derivative()], [integral()] and
#' [solve()]. Bounds, `b` and `target` are code rather than functions, so
#' every name in them is a symbol, and R values are injected with `!!`.
#'
#' @section Exact constants:
#' Maxima works with exact numbers where it can, but R evaluates `1/2` to the
#' decimal `0.5` before Maxima sees it. So that results stay exact, constants
#' *written* in the body of a function, in bounds and in `b` are kept exact:
#'
#' * literal fractions and powers of whole numbers, such as `1/2`, `2^-1` and
#'   `1/(2 * 3)`;
#' * `sqrt()`, `exp()` and `log()` of whole numbers (or of such fractions),
#'   such as `sqrt(2)` and `exp(1)`;
#' * `pi`, unless it is an argument of the function.
#'
#' Values which R has already computed arrive as decimals. This includes
#' values held in variables (`half <- 1/2`, used in a body as `x^half`) and
#' values injected with `!!` (`upper = !!half`). Maxima may keep them as
#' decimals (as [derivative()] does), or convert them to nearby fractions.
#' To keep a constant exact, write it literally.
#'
#' `Inf` and `-Inf` are only meaningful as a whole bound of [integral()]
#' (`upper = Inf`, or `upper = !!Inf`). They can't be part of a larger
#' expression in a bound, and are rejected in `b`.
#'
#' @examples
#' f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
#' f
#'
#' # Symbolic functions are ordinary functions, and can still be evaluated.
#' f(1, 0)
#'
#' # They can be transformed algebraically, producing new symbolic functions.
#' derivative(f, x)
#' derivative(f, x)(1, 0)
#'
#' @seealso [derivative()], [integral()], [solve.symbolic()]
#'
#' @export
as_symbolic <- function(f) {
  if (!is.function(f)) {
    abort(sprintf("`f` must be a function, not a %s.", typeof(f)))
  }
  if (is.primitive(f)) {
    abort("`f` must be a closure, not a primitive function.")
  }
  if ("..." %in% names(formals(f))) {
    abort("`f` must not use `...`, as symbolic arguments cannot be matched to it.")
  }
  if (is_symbolic(f)) {
    return(f)
  }
  structure(f, class = c("symbolic", oldClass(f)))
}

#' @rdname as_symbolic
#' @export
is_symbolic <- function(x) {
  inherits(x, "symbolic")
}

#' @rdname as_symbolic
#' @export
print.symbolic <- function(x, ...) {
  cat(sprintf(
    "{%s} -> %s\n",
    paste0(names(formals(x)), collapse = ", "),
    paste0(deparse(body(x)), collapse = "")
  ))
  invisible(x)
}
