#' Convert a Maxima result back into a symbolic R function
#'
#' Translates `x` into an R expression, and uses it as the body of a new
#' symbolic function taking `args` as its formals.
#'
#' Maxima returns solutions as a list of `target == value` equations. These are
#' unwrapped to their right hand sides, and multiple solutions are combined into
#' the columns of a matrix.
#'
#' @param x An `mx_expr` object.
#' @param nms The names in scope when `x` was created, used to translate
#'   placeholder symbols back into R names (see `maxima_symbols()`).
#' @param args An [alist()] of formals for the resulting function.
#' @param env The environment of the resulting function, usually that of the
#'   original function.
#' @param matrix Whether to always combine results into matrix columns with
#'   `cbind()`, even when there is only one. This keeps the result of `solve()`
#'   type-stable. Otherwise a single result is returned as is.
#'
#' @return A symbolic function.
#'
#' @noRd
r_from_maxima <- function(x, nms, args, env, matrix = FALSE) {
  body <- simplest_form(x)

  # Unevaluated noun forms (such as `integrate(f, x)`) mean Maxima could not
  # find a closed form. These would otherwise dispatch to unrelated R functions.
  unsolved <- intersect(all.names(body), c("integrate", "derivative", "limit", "sum"))
  if (length(unsolved) > 0L) {
    abort(sprintf(
      "Maxima could not find a closed form, returning an unevaluated `%s()`.",
      unsolved[[1L]]
    ))
  }

  placeholders <- lapply(nms, as.name)
  names(placeholders) <- maxima_placeholders(nms)
  body <- do.call(substitute, list(body, placeholders))

  # Handle multiple outputs as matrix.
  # `is_call()` is used over `call_name()` since the body may be a constant or
  # a symbol, which `call_name()` rejects.
  body <- if (is_call(body, "list")) {
    as.list(body[-1])
  } else {
    list(body)
  }

  # Remove "target ==" from solutions
  body <- lapply(body, function(x) {
    if (is_call(x, "==")) x[[3L]] else x
  })
  body <- lapply(body, tidy_expr)

  # Combine multiple results as matrix columns
  body <- if (matrix || length(body) > 1) {
    call2("cbind", !!!body)
  } else {
    body[[1L]]
  }

  as_symbolic(new_function(args, body, env))
}

#' Simplify a Maxima result into its most compact form
#'
#' Maxima only applies light simplification to results, so they can keep
#' cancelling terms (such as `rate * (1/rate - exp(-rate * q)/rate)` for
#' `1 - exp(-rate * q)`). No one simplification is best for all expressions:
#' `ratsimp()` combines fractions but keeps exponentials as a common factor,
#' while `expand()` cancels terms but multiplies out powers of sums. So `x` is
#' simplified by each, and the smallest result is kept, preferring `x` as it
#' is and then `ratsimp()` when there are ties. Each element of a list (such
#' as the solutions from `solve()`) is chosen separately.
#'
#' @param x An `mx_expr` object.
#'
#' @return An R expression, from [maxima::as_r_expr()].
#'
#' @noRd
simplest_form <- function(x) {
  candidates <- lapply(
    list(
      x,
      maxima::mx_call("ratsimp", x),
      maxima::mx_call("factor", x),
      maxima::mx_call("expand", x)
    ),
    maxima::as_r_expr
  )
  pick <- function(exprs) {
    exprs[[which.min(vapply(exprs, function(e) expr_size(tidy_expr(e)), numeric(1)))]]
  }
  if (!is_call(candidates[[1L]], "list")) {
    return(pick(candidates))
  }
  elements <- lapply(candidates, function(e) as.list(e[-1]))
  if (length(unique(lengths(elements))) != 1L) {
    return(candidates[[1L]])
  }
  call2("list", !!!lapply(seq_along(elements[[1L]]), function(i) {
    pick(lapply(elements, `[[`, i))
  }))
}

#' @noRd
expr_size <- function(x) {
  if (is_call(x, "(")) {
    expr_size(x[[2L]])
  } else if (is.call(x)) {
    sum(vapply(as.list(x), expr_size, numeric(1)))
  } else {
    1
  }
}

#' Tidy a Maxima result into idiomatic R code
#'
#' Maxima normalises expressions into sums and products of powers, so that
#' `x/y - 1` is returned as `-1 + x * y^-1`. This rewrites such expressions
#' into the form they would usually be written in R: negative powers become
#' division, negated terms become subtraction, constant terms are moved to the
#' end of sums, `e^x` becomes `exp(x)` and `x^(1/2)` becomes `sqrt(x)`.
#' Parentheses are added where needed, so the result matches the code a user
#' would write (and its [quote()]).
#'
#' Functions returned by Maxima which R lacks (such as `erf()`, `psi[n]()`,
#' `binomial()` and `bessel_j()`) are translated into their R equivalents.
#'
#' @param x An R expression.
#'
#' @return An R expression.
#'
#' @noRd
tidy_expr <- function(x) {
  tidy_call(normalise_expr(x))
}

#' Remove parentheses, and fold negated numbers into negative numbers
#'
#' Parsed code (such as a [quote()]) represents `-1` as a call to unary minus,
#' while [maxima::as_r_expr()] gives a negative number. Normalising both lets
#' `tidy_expr()` work on either.
#'
#' @noRd
normalise_expr <- function(x) {
  x <- strip_parens(x)
  if (!is.call(x)) {
    return(x)
  }
  if (is_call(x, "-", n = 1L) && is.numeric(strip_parens(x[[2L]]))) {
    return(-strip_parens(x[[2L]]))
  }
  x[-1] <- lapply(as.list(x[-1]), normalise_expr)
  x
}

#' @noRd
tidy_call <- function(x) {
  if (!is.call(x)) {
    return(x)
  }

  if (is_call(x, "+", n = 2L)) {
    return(tidy_sum(flatten_op(x, "+")))
  }
  if (is_call(x, "*", n = 2L)) {
    return(tidy_product(flatten_op(x, "*")))
  }
  if (is_call(x, "^", n = 2L)) {
    return(tidy_power(x[[2L]], x[[3L]]))
  }
  if (is_call(x[[1L]], "[", n = 2L) && identical(x[[1L]][[2L]], quote(psi)) && length(x) == 2L) {
    # Maxima's polygamma functions, psi[n](z)
    n <- x[[1L]][[3L]]
    z <- tidy_call(x[[2L]])
    return(switch(as.character(n),
      "0" = call2("digamma", z),
      "1" = call2("trigamma", z),
      call2("psigamma", z, n)
    ))
  }
  if (is_call(x, "erf", n = 1L)) {
    # erf(z) = 2 * pnorm(sqrt(2) * z) - 1
    z <- tidy_call(x[[2L]])
    return(tidy_call(call2("+", call2("*", 2, call2("pnorm", call2("*", quote(sqrt(2)), z))), -1)))
  }
  if (is_call(x, "erfc", n = 1L)) {
    # erfc(z) = 2 * pnorm(-sqrt(2) * z)
    z <- tidy_call(x[[2L]])
    return(tidy_call(call2("*", 2, call2("pnorm", call2("*", quote(-sqrt(2)), z)))))
  }
  if (is_call(x, c("binomial", "signum", "truncate"))) {
    x[[1L]] <- as.name(switch(as.character(x[[1L]]),
      binomial = "choose", signum = "sign", truncate = "trunc"
    ))
  }
  if (is_call(x, c("bessel_j", "bessel_y", "bessel_i", "bessel_k"), n = 2L)) {
    # Maxima's bessel_j(nu, x) is R's besselJ(x, nu)
    fn <- paste0("bessel", toupper(substring(as.character(x[[1L]]), 8L)))
    x <- call2(fn, x[[3L]], x[[2L]])
  }
  reciprocal <- c(sec = "cos", csc = "sin", cot = "tan", sech = "cosh", csch = "sinh", coth = "tanh")
  if (is_call(x, names(reciprocal), n = 1L)) {
    # Reciprocal trigonometric functions, such as sec(x) = 1/cos(x)
    return(binary_op("/", 1, call2(reciprocal[[as.character(x[[1L]])]], tidy_call(x[[2L]]))))
  }

  x[-1] <- lapply(as.list(x[-1]), tidy_call)
  x
}

#' @noRd
tidy_sum <- function(terms) {
  terms <- lapply(terms, tidy_call)

  # Constant terms are moved to the end, as in `2 * x + 1`.
  constant <- vapply(terms, is_constant, logical(1))
  terms <- c(terms[!constant], terms[constant])

  # Unless every other term is negated, when a positive constant leads, as in
  # `1 - exp(-x)`.
  constant <- sort(constant)
  negated <- vapply(terms, is_negated, logical(1))
  if (any(constant & !negated) && all(negated[!constant])) {
    first <- which(constant & !negated)[[1L]]
    terms <- c(terms[first], terms[-first])
  }

  out <- terms[[1L]]
  for (term in terms[-1]) {
    out <- if (is_negated(term)) {
      binary_op("-", out, negate(term))
    } else {
      binary_op("+", out, term)
    }
  }
  out
}

#' @noRd
tidy_product <- function(factors) {
  coef <- 1
  numer <- list()
  denom <- list()

  for (f in factors) {
    if (is.numeric(f)) {
      coef <- coef * f
    } else if (is_rational(f)) {
      coef <- coef * f[[2L]]
      denom <- c(denom, list(f[[3L]]))
    } else if (is_call(f, "^", n = 2L) && is_negative_number(f[[3L]])) {
      denom <- c(denom, list(tidy_power(f[[2L]], negate_number(f[[3L]]))))
    } else {
      numer <- c(numer, list(tidy_call(f)))
    }
  }

  negative <- coef < 0
  coef <- abs(coef)
  if (coef != 1 || length(numer) == 0L) {
    numer <- c(list(coef), numer)
  }

  # The sign is applied to the leading factor, as in `-2 * x`.
  if (negative) {
    numer[[1L]] <- if (is.numeric(numer[[1L]])) -numer[[1L]] else unary_minus(numer[[1L]])
  }
  out <- Reduce(function(a, b) binary_op("*", a, b), numer)
  if (length(denom) > 0L) {
    out <- binary_op("/", out, Reduce(function(a, b) binary_op("*", a, b), denom))
  }
  out
}

#' @noRd
tidy_power <- function(base, exponent) {
  if (is_negative_number(exponent)) {
    return(binary_op("/", 1, tidy_power(base, negate_number(exponent))))
  }
  if (is_rational(exponent) && identical(exponent[[2L]], 1) && identical(exponent[[3L]], 2)) {
    return(call2("sqrt", tidy_call(base)))
  }
  if (identical(exponent, 1)) {
    return(tidy_call(base))
  }
  if (identical(base, quote(exp(1)))) {
    return(call2("exp", tidy_call(exponent)))
  }
  binary_op("^", tidy_call(base), tidy_call(exponent))
}

#' Flatten nested calls to an associative operator into a list of operands
#' @noRd
flatten_op <- function(x, op) {
  if (!is_call(x, op, n = 2L)) {
    return(list(x))
  }
  c(flatten_op(x[[2L]], op), flatten_op(x[[3L]], op))
}

#' @noRd
is_rational <- function(x) {
  is_call(x, "/", n = 2L) && is.numeric(x[[2L]]) && is.numeric(x[[3L]])
}

#' @noRd
is_negative_number <- function(x) {
  (is.numeric(x) && x < 0) || (is_rational(x) && x[[2L]] < 0)
}

#' @noRd
negate_number <- function(x) {
  if (is.numeric(x)) -x else call2("/", -x[[2L]], x[[3L]])
}

#' @noRd
is_constant <- function(x) {
  is.numeric(x) || is_rational(x) || is_rational(strip_minus(x))
}

#' @noRd
is_negated <- function(x) {
  (is.numeric(x) && x < 0) || is_call(x, "-", n = 1L) ||
    (is_call(x, c("*", "/"), n = 2L) && is_negated(x[[2L]]))
}

#' @noRd
negate <- function(x) {
  if (is.numeric(x)) {
    return(-x)
  }
  if (is_call(x, "-", n = 1L)) {
    return(strip_parens(x[[2L]]))
  }
  if (is_call(x, c("*", "/"), n = 2L)) {
    op <- as.character(x[[1L]])
    return(binary_op(op, negate(strip_parens(x[[2L]])), strip_parens(x[[3L]])))
  }
  unary_minus(x)
}

#' @noRd
strip_minus <- function(x) {
  if (is_call(x, "-", n = 1L)) strip_parens(x[[2L]]) else x
}

#' @noRd
strip_parens <- function(x) {
  while (is_call(x, "(")) {
    x <- x[[2L]]
  }
  x
}

#' Construct operator calls, adding parentheses where needed
#'
#' Calls constructed programmatically don't contain the `(` calls that parsed
#' code does, so these are added to match the code a user would write.
#'
#' @noRd
binary_op <- function(op, lhs, rhs) {
  lhs <- strip_parens(lhs)
  rhs <- strip_parens(rhs)
  prec <- op_precedence(op)

  # `^` is right associative, the others are left associative.
  if (op == "^") {
    if (op_precedence(lhs) <= prec || (is.numeric(lhs) && lhs < 0)) lhs <- call2("(", lhs)
    if (op_precedence(rhs) < prec) rhs <- call2("(", rhs)
  } else {
    if (op_precedence(lhs) < prec) lhs <- call2("(", lhs)
    if (op_precedence(rhs) <= prec) rhs <- call2("(", rhs)
  }
  call2(op, lhs, rhs)
}

#' @noRd
unary_minus <- function(x) {
  x <- strip_parens(x)
  if (op_precedence(x) < op_precedence("-x")) {
    x <- call2("(", x)
  }
  call2("-", x)
}

#' @noRd
op_precedence <- function(x) {
  if (is.character(x)) {
    op <- x
  } else if (is_call(x, "-", n = 1L)) {
    op <- "-x"
  } else if (is.call(x) && is.name(x[[1L]]) && length(x) == 3L) {
    op <- as.character(x[[1L]])
  } else if (is.numeric(x) && x < 0) {
    # Negative numbers deparse as a unary minus
    op <- "-x"
  } else {
    return(Inf)
  }
  switch(op,
    "+" = , "-" = 1,
    "*" = , "/" = 2,
    "-x" = 3,
    "^" = 4,
    Inf
  )
}

#' Default target argument for a symbolic function
#'
#' @param f A symbolic function.
#'
#' @return The name of the first argument of `f`.
#'
#' @noRd
default_target <- function(f) {
  nms <- names(formals(f))
  if (length(nms) == 0L) {
    abort("`f` must have at least one argument to be manipulated symbolically.")
  }
  nms[[1L]]
}

#' Capture a target argument
#'
#' `target` is the bare name of an argument of `f`, such as `x`. A string is
#' also accepted, since it names an argument rather than holding code, which
#' allows injecting a name held in a variable with `!!`.
#'
#' @param quo A quosure, from `enquo(target)`.
#' @param f A symbolic function.
#' @param missing Whether `target` was supplied, from `missing(target)`. The
#'   default is the first argument of `f`.
#' @param bounds_call For functions taking bounds after `target` (such as
#'   `integral()`), the call being made, from `sys.call()`. When `target` looks
#'   like a bound (a number or an expression), as in `integral(f, 0, 1)`, the
#'   error suggests the call with the default target inserted before it.
#'
#' @return The name of the target argument, as a string.
#'
#' @noRd
capture_target <- function(quo, f, missing, bounds_call = NULL) {
  if (!is_symbolic(f)) {
    abort("`f` must be a symbolic function, as created by `as_symbolic()`.", call = NULL)
  }
  if (missing) {
    return(default_target(f))
  }

  target <- quo_squash(quo)
  if (is_string(target) && nzchar(target)) {
    target <- as.name(target)
  }
  if (!is.name(target)) {
    bound_like <- is.numeric(target) || is.call(target)
    hint <- if (!is.null(bounds_call) && bound_like && !("target" %in% names(bounds_call))) {
      c(i = sprintf(
        "`target` comes before the bounds. Did you mean `%s`?",
        target_hint(bounds_call, default_target(f))
      ))
    }
    abort(c(
      "`target` must be the name of an argument of `f`, such as `x`.",
      hint,
      i = "Use `!!` to inject a name held in a variable, as in `!!name`."
    ), call = NULL)
  }

  target <- as.character(target)
  nms <- names(formals(f))
  if (!(target %in% nms)) {
    abort(sprintf(
      "`target` must be an argument of `f`.\n* `target` is `%s`, but `f` takes %s.",
      target,
      paste0("`", nms, "`", collapse = ", ")
    ), call = NULL)
  }
  target
}

#' Suggest a call with the target inserted before the bounds
#'
#' Rewrites a call such as `integral(f, 0, 1)` into `integral(f, x, 0, 1)`,
#' for the error when a bound is passed as `target` by position.
#'
#' @param call The call made, from `sys.call()`.
#' @param target The name of the default target.
#'
#' @return The suggested call, as a string.
#'
#' @noRd
target_hint <- function(call, target) {
  args <- as.list(call[-1])
  arg_nms <- rlang::names2(args)
  f_pos <- if ("f" %in% arg_nms) which(arg_nms == "f") else which(arg_nms == "")[[1L]]
  # The function argument is shown as `f`, rather than as it was written.
  # Within a method, `sys.call()` names the method rather than the generic.
  fn <- call[[1L]]
  if (is.name(fn)) {
    fn <- as.name(sub("\\.symbolic$", "", as.character(fn)))
  }
  call <- call2(fn, quote(f), as.name(target), !!!args[-f_pos])
  paste(deparse(call, width.cutoff = 500L), collapse = " ")
}
