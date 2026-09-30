#' Differentiate a symbolic function
#'
#' @description
#' Computes the derivative of a symbolic function with respect to one of its
#' arguments, returning the derivative as a new symbolic function.
#'
#' @param f A symbolic function, as created by [as_symbolic()].
#' @param target The argument to differentiate with respect to, as a bare
#'   name such as `x`. Defaults to the first argument of `f`. A string naming
#'   the argument also works, which allows injecting a name held in a variable
#'   with `!!`.
#' @param ... Not used.
#'
#' @return A symbolic function with the same arguments as `f`, returning the
#'   derivative of `f` with respect to `target`.
#'
#' @examples
#' f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
#'
#' derivative(f, x)
#' derivative(f, y)
#'
#' # The derivative is a function, and can be evaluated.
#' derivative(f, x)(1, 0)
#'
#' # `target` defaults to the first argument of `f`.
#' derivative(as_symbolic(function(x) x^3))
#'
#' # Derivatives can be taken repeatedly.
#' derivative(derivative(f, x), x)
#'
#' # Inject a name held in a variable with `!!`.
#' var <- "y"
#' derivative(f, !!var)
#'
#' @seealso [integral()], [solve.symbolic()]
#'
#' @export
derivative <- function(f, target, ...) {
  UseMethod("derivative")
}

#' @rdname derivative
#' @export
derivative.symbolic <- function(f, target = default_target(f), ...) {
  target <- capture_target(enquo(target), f, missing(target))
  nms <- names(formals(f))
  expr <- as_maxima(f, nms)

  res <- maxima_call("diff", expr, maxima_symbols(nms)[[target]], nms = nms)
  r_from_maxima(res, nms, formals(f), environment(f))
}

#' Integrate a symbolic function
#'
#' @description
#' Computes the integral of a symbolic function with respect to one of its
#' arguments, returning the integral as a new symbolic function.
#'
#' If `lower` and `upper` are omitted, the indefinite integral (the
#' antiderivative) is returned, and the result keeps all arguments of `f`. If
#' both are given, the definite integral is returned, and `target` is dropped
#' from the result's arguments since the integral no longer depends on it.
#'
#' @section Bounds:
#' `lower` and `upper` are bare R expressions, such as `0`, `Inf` or
#' `2 * sqrt(y)`. They are captured rather than evaluated, and every name in
#' them is a symbol:
#'
#' * Names which are arguments of `f` refer to those arguments, so
#'   `upper = 2 * y` integrates up to twice `y`.
#' * Other names are new symbols, which become arguments of the result. They
#'   are added before the arguments of `f`, in order of first appearance, so
#'   `integral(f, x, 0, b)` returns a function of `b` and the remaining
#'   arguments of `f`.
#' * A name is a symbol even if a variable of that name exists. To use the
#'   value of a variable, inject it with `!!`, as in `lower = !!lo`. Maxima
#'   expressions can be injected the same way.
#' * If a bound mentions `target` itself, as in `upper = x` when integrating
#'   over `x`, the integral is taken over a dummy variable and `target` stays
#'   an argument of the result. For example, this computes a cumulative
#'   distribution function from a density.
#'
#' Functions in bounds are called as usual, and `pi` and fractions written as
#' literals (such as `1/2`) are exact, as in the body of a symbolic function.
#' `Inf` and `-Inf` are infinite bounds, and can't be part of a larger
#' expression. Strings are not accepted: write `upper = 2 * y`, not
#' `upper = "2 * y"`.
#'
#' @inheritParams derivative
#' @param target The argument to integrate with respect to, as a bare name
#'   such as `x`. Defaults to the first argument of `f`. A string naming the
#'   argument also works, which allows injecting a name held in a variable
#'   with `!!`.
#' @param lower,upper The limits of a definite integral, as bare R expressions
#'   (see the Bounds section). Both must be given together, or both omitted for
#'   an indefinite integral. Names are symbols, and R values are injected with
#'   `!!`. Symbols which aren't arguments of `f` become arguments of the
#'   result.
#'
#' @return A symbolic function returning the integral of `f` with respect to
#'   `target`.
#'
#' @examples
#' f <- as_symbolic(function(x, y) x^2 + x * y)
#'
#' # Indefinite integrals keep all arguments of `f`.
#' integral(f, x)
#'
#' # Definite integrals drop `target`, as the result no longer depends on it.
#' integral(f, x, lower = 0, upper = 1)
#' integral(f, x, lower = 0, upper = 1)(0)
#'
#' # Bounds can be infinite, or expressions of other arguments.
#' integral(as_symbolic(function(x) exp(-x)), x, lower = 0, upper = Inf)
#' integral(f, x, lower = 0, upper = 2 * y)
#'
#' # New symbols in bounds become arguments of the result.
#' integral(f, x, lower = 0, upper = b)
#'
#' # Names are always symbols, so use `!!` to inject the value of a variable.
#' lo <- 1
#' integral(f, x, lower = lo, upper = 2)
#' integral(f, x, lower = !!lo, upper = 2)
#'
#' # Integrating a density up to `x` gives a cumulative distribution function.
#' dens <- as_symbolic(function(x, rate) rate * exp(-rate * x))
#' cdf <- integral(dens, x, lower = 0, upper = x)
#' cdf
#' cdf(1, rate = 2)
#'
#' # Integration undoes differentiation.
#' integral(derivative(f, x), x)
#'
#' @seealso [derivative()], [solve.symbolic()]
#'
#' @export
integral <- function(f, target, ...) {
  UseMethod("integral")
}

#' @rdname integral
#' @export
integral.symbolic <- function(f, target = default_target(f), lower, upper, ...) {
  target <- capture_target(enquo(target), f, missing(target), bounds_call = sys.call())

  if (missing(lower) != missing(upper)) {
    abort("`lower` and `upper` must both be supplied for a definite integral.")
  }
  nms <- names(formals(f))
  args <- formals(f)

  if (missing(lower)) {
    expr <- as_maxima(f, nms)
    res <- maxima_call("integrate", expr, maxima_symbols(nms)[[target]], nms = nms)
    return(r_from_maxima(res, nms, args, environment(f)))
  }

  lower <- enquo(lower)
  upper <- enquo(upper)
  lower_expr <- capture_expr(lower, "lower")
  upper_expr <- capture_expr(upper, "upper")

  # Every name in a bound is a symbol. Those which aren't already arguments of
  # `f` become arguments of the result, in order of first appearance.
  bound_nms <- unique(c(expr_symbols(lower_expr, nms), expr_symbols(upper_expr, nms)))
  new_nms <- setdiff(bound_nms, nms)
  nms <- c(nms, new_nms)

  expr <- as_maxima(f, nms)
  lower <- expr_to_maxima(lower_expr, nms, quo_get_env(lower), "lower")
  upper <- expr_to_maxima(upper_expr, nms, quo_get_env(upper), "upper")
  var <- maxima_symbols(nms)[[target]]

  # A definite integral no longer depends on the variable of integration,
  # unless a bound does (as in integrating up to `x`). Then the integral is
  # taken over a dummy variable, and `target` stays an argument.
  if (target %in% bound_nms) {
    # The dummy is reported by the name of `target` in Maxima's questions.
    nms <- c(nms, target)
    dummy <- maxima::mx_symbol(maxima_placeholders(nms)[[length(nms)]])
    expr <- maxima_call("substitute", dummy, var, expr, nms = nms)
    var <- dummy
  } else {
    args <- args[setdiff(names(args), target)]
  }

  new_args <- rep(list(quote(expr = )), length(new_nms))
  names(new_args) <- new_nms
  args <- c(new_args, args)

  res <- maxima_call("integrate", expr, var, lower, upper, nms = nms)
  r_from_maxima(res, nms, args, environment(f))
}

#' Solve a symbolic function
#'
#' @description
#' Solves `a(...) == b` for one of the arguments of `a`, returning the solution
#' as a new symbolic function. If `b` is omitted, the inverse of `a` is
#' returned instead.
#'
#' The arguments follow [base::solve()], which solves `a %*% x = b` and returns
#' the inverse of `a` when `b` is missing. This is why the function to solve is
#' named `a`, and why `b` comes before `target`: to solve for a particular
#' argument without `b`, name it, as in `solve(f, target = x)`.
#'
#' @section Inverses:
#' Without `b`, `solve()` returns the inverse of `a` with respect to `target`:
#' the function giving the value of `target` for which `a(...)` equals a given
#' value. As is usual for inverses, that value takes the name and position of
#' `target` in the arguments, so the inverse of `function(x) exp(x)` is
#' `function(x) log(x)`. The other arguments of `a` are kept, so the inverse
#' of `function(x, y) x^3 + y` with respect to `x` is a function of `x` (the
#' value of `a`) and `y`.
#'
#' @param a A symbolic function, as created by [as_symbolic()].
#' @param b The value to solve against, as a bare R expression such as `0`,
#'   `z` or `2 * y`. As with the bounds of [integral()], `b` is captured rather
#'   than evaluated and every name in it is a symbol: names which aren't
#'   arguments of `a` become arguments of the result, and R values are injected
#'   with `!!`, as in `b = !!value`. Strings are not accepted. If omitted, the
#'   inverse of `a` is returned (see the Inverses section).
#' @param target The argument to solve for, as a bare name such as `x`.
#'   Defaults to the first argument of `a`. A string naming the argument also
#'   works, which allows injecting a name held in a variable with `!!`.
#' @param ... Not used.
#'
#' @return A symbolic function returning the values of `target` which solve
#'   `a(...) == b`. Its arguments are those of `a` other than `target`, after
#'   any new symbols in `b`. For an inverse (with `b` omitted), the arguments
#'   are those of `a`, with `target` now standing for the value of `a`.
#'
#'   The result always returns a matrix, with one column per solution in the
#'   order Maxima finds them, and one row per element of vectorised input. This
#'   holds even when there is only one solution, so results have the same shape
#'   however many solutions are found. An error is raised if no solutions are
#'   found, or if `target` can only be solved implicitly.
#'
#' @examples
#' f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
#'
#' # Solve log(2x + 1 + y) == 0 for x.
#' solve(f, 0, x)
#' solve(f, 0, x)(1)
#'
#' # Solve against a new symbol, which becomes an argument of the result.
#' solve(f, z, x)
#'
#' # Or against an expression of the arguments.
#' solve(f, 2 * y, x)
#'
#' # Names are always symbols, so use `!!` to inject the value of a variable.
#' level <- 1
#' solve(f, !!level, x)
#'
#' # Results are matrices, with a column per solution and a row per input.
#' solve(as_symbolic(function(x) x^2), y, x)
#' solve(as_symbolic(function(x) x^2), y, x)(c(1, 4, 9))
#'
#' # Without `b`, the inverse is returned.
#' solve(as_symbolic(function(x) exp(x)))
#' solve(f, target = x)
#'
#' @seealso [derivative()], [integral()]
#'
#' @export
solve.symbolic <- function(a, b, target = default_target(a), ...) {
  target <- capture_target(enquo(target), a, missing(target))
  nms <- names(formals(a))
  args <- formals(a)

  if (missing(b)) {
    # The inverse solves `a(...) == value` for `target`, where the value takes
    # the name and position of `target`. It has its own placeholder, so it
    # stays distinct from `target` in Maxima, while being reported by the name
    # of `target` in Maxima's questions.
    nms <- c(nms, target)
    rhs <- maxima::mx_symbol(maxima_placeholders(nms)[[length(nms)]])
    # A default for `target` was a default input of `a`, not of its value.
    args[target] <- list(quote(expr = ))
  } else {
    b <- enquo(b)
    b_expr <- capture_expr(b, "b")

    # Every name in `b` is a symbol. Those which aren't already arguments of
    # `a` become arguments of the result, in order of first appearance.
    new_nms <- setdiff(expr_symbols(b_expr, nms), nms)
    nms <- c(nms, new_nms)

    new_args <- rep(list(quote(expr = )), length(new_nms))
    names(new_args) <- new_nms
    args <- c(new_args, args[setdiff(names(args), target)])

    rhs <- expr_to_maxima(b_expr, nms, quo_get_env(b), "b")
  }

  expr <- as_maxima(a, nms)
  var <- maxima_symbols(nms)[[target]]
  res <- maxima_call("solve", expr == rhs, var, nms = nms)

  solutions <- maxima::as_r_expr(res)
  if (length(solutions) <= 1L) {
    abort("No solutions were found.")
  }
  # Solutions are only implicit if they aren't of the form `target == value`
  # (as when Maxima returns `x^x == 2` unchanged), or the value still depends
  # on the target.
  target_placeholder <- maxima_placeholders(nms)[[match(target, nms)]]
  implicit <- vapply(
    as.list(solutions[-1]),
    function(x) {
      !identical(x[[2L]], as.name(target_placeholder)) ||
        target_placeholder %in% all.vars(x[[3L]])
    },
    logical(1)
  )
  if (any(implicit)) {
    abort(sprintf("`%s` could not be solved for explicitly.", target))
  }

  r_from_maxima(res, nms, args, environment(a), matrix = TRUE)
}
