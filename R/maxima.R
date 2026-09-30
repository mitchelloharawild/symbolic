#' Maxima symbols standing in for R names
#'
#' Maxima is case-insensitive, and reserves several names which are ordinary
#' symbols in R (such as `inf`, `und` and `true`). To keep every R name distinct
#' and free of Maxima's meaning, each name is represented in Maxima by a
#' placeholder symbol, and translated back by `r_from_maxima()`.
#'
#' Placeholders are assigned by position, so extending `nms` keeps the
#' placeholders of existing names.
#'
#' @param nms A character vector of R names.
#'
#' @return A named list of Maxima symbols, named by `nms`.
#'
#' @noRd
maxima_symbols <- function(nms) {
  syms <- lapply(maxima_placeholders(nms), maxima::mx_symbol)
  names(syms) <- nms
  syms
}

#' @noRd
maxima_placeholders <- function(nms) {
  sprintf("symbolic_%i", seq_along(nms))
}

#' Call a Maxima function
#'
#' A wrapper around [maxima::mx_call()] which translates placeholder symbols in
#' Maxima's errors back into R names (see `maxima_symbols()`).
#'
#' Maxima sometimes needs to know more about a symbol to continue, such as
#' whether it is positive when computing a definite integral. It can't ask this
#' from R, so the question is reported as an error.
#'
#' @param name The name of the Maxima function.
#' @param ... Arguments passed to [maxima::mx_call()].
#' @param nms The names in scope.
#'
#' @return An `mx_expr` object.
#'
#' @noRd
maxima_call <- function(name, ..., nms) {
  tryCatch(
    maxima::mx_call(name, ...),
    error = function(e) {
      msg <- rename_placeholders(conditionMessage(e), nms)
      question <- regmatches(msg, regexec('asked: "([^"]*)"', msg))[[1L]]
      if (length(question) == 2L) {
        abort(c(
          sprintf("Maxima needs more information to continue, and asked: \"%s\"", question[[2L]]),
          i = "symbolic can't pass assumptions about arguments to Maxima yet."
        ), call = NULL)
      }
      abort(msg, call = NULL)
    }
  )
}

#' Replace placeholder symbol names in text with their R names
#' @noRd
rename_placeholders <- function(text, nms) {
  m <- gregexpr("\\bsymbolic_[0-9]+\\b", text)
  regmatches(text, m) <- lapply(regmatches(text, m), function(x) {
    vapply(x, function(placeholder) {
      i <- as.integer(sub("symbolic_", "", placeholder, fixed = TRUE))
      if (i <= length(nms)) nms[[i]] else placeholder
    }, character(1), USE.NAMES = FALSE)
  })
  text
}

#' Evaluate a function symbolically with Maxima
#'
#' Calls `f` with each of its formals replaced by a Maxima symbol, capturing
#' the function's body as a Maxima expression. Default values of formals are
#' ignored: every formal is treated as a free symbol.
#'
#' Other names in the body follow R's rules for closures, and are evaluated in
#' the environment of `f`. A name which isn't defined gives an error suggesting
#' to make it an argument of `f` (see `undefined_name_error()`).
#'
#' Fractions written as literals in the body of `f` (such as `1/2`) are kept
#' exact, see `exact_constants()`. R functions and constants which aren't
#' understood by Maxima are masked with Maxima equivalents, see
#' `maxima_functions()`.
#'
#' Infinite and undefined values (`Inf`, `-Inf` and `NaN`) are an error, see
#' `non_finite_body_error()`.
#'
#' @param f A function.
#' @param nms The names in scope, which must start with the formals of `f`.
#'
#' @return An `mx_expr` object.
#'
#' @noRd
as_maxima <- function(f, nms = names(formals(f))) {
  if (length(formals(f)) == 0L) {
    abort("`f` must have at least one argument to be manipulated symbolically.")
  }
  syms <- maxima_symbols(nms)
  fn_body <- body(f)
  if (has_infinity(fn_body)) {
    non_finite_body_error()
  }
  body(f) <- exact_constants(fn_body)
  environment(f) <- list2env(maxima_functions(), parent = environment(f))
  res <- withCallingHandlers(
    do.call(f, syms[names(formals(f))]),
    error = function(e) undefined_name_error(e, fn_body, names(formals(f)))
  )

  # Functions which ignore their arguments (such as constants) return a plain
  # R value rather than a Maxima expression.
  res <- maxima::as_mx_expr(res)
  if (has_non_finite(maxima::as_r_expr(res))) {
    non_finite_body_error()
  }
  res
}

#' Reject infinite and undefined values in the body of a function
#'
#' R's `Inf`, `-Inf` and `NaN` reach Maxima as floating point values, which
#' Maxima's simplifier treats as ordinary numbers. This silently gives wrong
#' results: `Inf * x` is differentiated to `Inf`, ignoring that its sign
#' depends on `x` and that it is undefined at `x = 0`, and `Inf * x * 0`
#' simplifies to `0`. Some operations (such as `solve()`) fail inside Maxima.
#' Mapping them to Maxima's `inf` and `minf` doesn't help, as these are only
#' meaningful as limits (and as bounds of `integral()`, see `expr_to_maxima()`),
#' and Maxima's simplifier treats them as ordinary symbols elsewhere.
#'
#' `as_maxima()` checks the body before evaluating it, since Maxima may
#' simplify infinities away (as in `Inf * x * 0`), and checks the result for
#' non-finite values from elsewhere (such as a variable holding `Inf`).
#'
#' @noRd
non_finite_body_error <- function() {
  abort(c(
    "`f` can't be manipulated symbolically, as its body contains an infinite or undefined value (`Inf`, `-Inf` or `NaN`).",
    i = "Arithmetic with infinity has no single answer, as the sign of `Inf * x` depends on `x` and it is undefined at `x = 0`.",
    i = "`Inf` and `-Inf` are only meaningful as a bound of `integral()`."
  ), call = NULL)
}

#' Explain an undefined name in the body of a function
#'
#' Names in the body of a symbolic function which aren't arguments are
#' evaluated as R code, so a name meant as a symbol (such as `k` in
#' `function(x) k * x^2`) gives R's "object not found" error. This rethrows
#' that error with a suggestion to make the name an argument. Only errors about
#' names which appear in the body are rethrown, and all other errors are left
#' to propagate unchanged.
#'
#' @param e The error condition.
#' @param body The body of the function, before `exact_constants()`.
#' @param args The names of the function's arguments.
#'
#' @return `NULL`, when `e` is left to propagate.
#'
#' @noRd
undefined_name_error <- function(e, body, args) {
  name <- regmatches(
    conditionMessage(e),
    regexec("^object '(.*)' not found$", conditionMessage(e))
  )[[1L]]
  if (length(name) != 2L || !(name[[2L]] %in% all.vars(body))) {
    return(NULL)
  }
  name <- name[[2L]]
  formals <- rep(list(quote(expr = )), length(args) + 1L)
  names(formals) <- c(args, name)
  example <- call2("function", as.pairlist(formals), body)
  abort(c(
    sprintf("`%s` in the body of `f` isn't an argument, and no R object called `%s` was found.", name, name),
    i = "Names in the body which aren't arguments are evaluated as R code, following R's rules for closures.",
    i = sprintf(
      "To use `%s` as a symbol, make it an argument, as in `%s`.",
      name, paste(deparse(example, width.cutoff = 500L), collapse = " ")
    )
  ), call = NULL)
}

#' Maxima equivalents of R functions and constants
#'
#' R functions which Maxima doesn't share, but which `tidy_expr()` produces
#' from Maxima results, are masked while tracing a function so that results can
#' be manipulated again. `cbind()`, which combines the solutions from `solve()`,
#' returns a single solution as it is, and is an error for several. Each falls back to the R function when none of its
#' arguments are Maxima expressions. `pi` is masked with Maxima's exact `%pi`.
#'
#' Maxima expressions only dispatch R's `Math` group generic (such as `sin()`),
#' passing the R name to Maxima unchanged. Functions outside the group (such as
#' `beta()` and `choose()`) would otherwise be evaluated by R, and those in the
#' group which Maxima names differently (such as `lgamma()` and `sign()`) or
#' whose extra arguments would be dropped (such as `log(x, base)`) are masked
#' with their Maxima equivalents.
#'
#' @return A named list of functions and constants.
#'
#' @noRd
maxima_functions <- function() {
  is_mx <- function(...) any(vapply(list(...), inherits, logical(1), "mx_expr"))

  # Maxima's psi[n](z), constructed via the derivative of log(gamma(z)) as
  # subscripted functions can't be called directly.
  polygamma <- function(z, n) {
    t <- maxima::mx_symbol("symbolic_dummy")
    maxima::mx_call("substitute", z, t, maxima::mx_call("diff", log(gamma(t)), t, n + 1L))
  }

  list(
    pi = maxima::mx_symbol("%pi"),
    # Results of `solve()` have a column per solution. A single solution is a
    # single expression, while several can't be manipulated as one.
    cbind = function(...) {
      if (...length() == 1L) {
        return(..1)
      }
      abort(c(
        sprintf(
          "`f` returns %i solutions (as the columns of `cbind()`), so it can't be manipulated as a single expression.",
          ...length()
        ),
        i = "To manipulate one solution, create a symbolic function from its column of the body."
      ), call = NULL)
    },
    digamma = function(x) {
      if (is_mx(x)) polygamma(x, 0L) else base::digamma(x)
    },
    trigamma = function(x) {
      if (is_mx(x)) polygamma(x, 1L) else base::trigamma(x)
    },
    psigamma = function(x, deriv = 0L) {
      if (is_mx(x)) polygamma(x, deriv) else base::psigamma(x, deriv)
    },
    pnorm = function(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
      if (!is_mx(q, mean, sd)) {
        return(stats::pnorm(q, mean, sd, lower.tail, log.p))
      }
      z <- (q - mean) / (sd * maxima::mx_call("sqrt", 2L))
      p <- if (lower.tail) {
        (1 + maxima::mx_call("erf", z)) / 2
      } else {
        maxima::mx_call("erfc", z) / 2
      }
      if (log.p) log(p) else p
    },
    # Functions outside the `Math` group generic, which R evaluates directly.
    beta = function(a, b) {
      if (is_mx(a, b)) maxima::mx_call("beta", a, b) else base::beta(a, b)
    },
    lbeta = function(a, b) {
      if (is_mx(a, b)) log(maxima::mx_call("beta", a, b)) else base::lbeta(a, b)
    },
    choose = function(n, k) {
      if (is_mx(n, k)) maxima::mx_call("binomial", n, k) else base::choose(n, k)
    },
    lchoose = function(n, k) {
      if (is_mx(n, k)) log(maxima::mx_call("binomial", n, k)) else base::lchoose(n, k)
    },
    lfactorial = function(x) {
      if (is_mx(x)) log(gamma(x + 1L)) else base::lfactorial(x)
    },
    atan2 = function(y, x) {
      if (is_mx(y, x)) maxima::mx_call("atan2", y, x) else base::atan2(y, x)
    },
    besselJ = function(x, nu) {
      if (is_mx(x, nu)) maxima::mx_call("bessel_j", nu, x) else base::besselJ(x, nu)
    },
    besselY = function(x, nu) {
      if (is_mx(x, nu)) maxima::mx_call("bessel_y", nu, x) else base::besselY(x, nu)
    },
    besselI = function(x, nu, expon.scaled = FALSE) {
      if (!is_mx(x, nu)) {
        return(base::besselI(x, nu, expon.scaled))
      }
      res <- maxima::mx_call("bessel_i", nu, x)
      if (expon.scaled) exp(-x) * res else res
    },
    besselK = function(x, nu, expon.scaled = FALSE) {
      if (!is_mx(x, nu)) {
        return(base::besselK(x, nu, expon.scaled))
      }
      res <- maxima::mx_call("bessel_k", nu, x)
      if (expon.scaled) exp(x) * res else res
    },
    # Functions in the `Math` group generic, which Maxima lacks or names
    # differently, or whose extra arguments would be dropped.
    log = function(x, base = exp(1)) {
      if (!is_mx(x, base)) {
        return(base::log(x, base))
      }
      if (missing(base)) {
        return(maxima::mx_call("log", x))
      }
      maxima::mx_call("log", x) / maxima::mx_call("log", base)
    },
    log2 = function(x) {
      if (is_mx(x)) maxima::mx_call("log", x) / maxima::mx_call("log", 2L) else base::log2(x)
    },
    log10 = function(x) {
      if (is_mx(x)) maxima::mx_call("log", x) / maxima::mx_call("log", 10L) else base::log10(x)
    },
    log1p = function(x) {
      if (is_mx(x)) log(1L + x) else base::log1p(x)
    },
    expm1 = function(x) {
      if (is_mx(x)) exp(x) - 1L else base::expm1(x)
    },
    lgamma = function(x) {
      if (is_mx(x)) log(gamma(x)) else base::lgamma(x)
    },
    sinpi = function(x) {
      if (is_mx(x)) sin(maxima::mx_symbol("%pi") * x) else base::sinpi(x)
    },
    cospi = function(x) {
      if (is_mx(x)) cos(maxima::mx_symbol("%pi") * x) else base::cospi(x)
    },
    tanpi = function(x) {
      if (is_mx(x)) tan(maxima::mx_symbol("%pi") * x) else base::tanpi(x)
    },
    # Maxima's `sign()` describes the sign of an expression (such as `pnz`),
    # and its `trunc()` is a display function.
    sign = function(x) {
      if (is_mx(x)) maxima::mx_call("signum", x) else base::sign(x)
    },
    trunc = function(x, ...) {
      if (is_mx(x)) maxima::mx_call("truncate", x) else base::trunc(x, ...)
    },
    round = function(x, digits = 0) {
      if (!is_mx(x, digits)) {
        return(base::round(x, digits))
      }
      if (missing(digits)) {
        return(maxima::mx_call("round", x))
      }
      scale <- maxima::as_mx_expr(10L)^digits
      maxima::mx_call("round", x * scale) / scale
    }
  )
}

#' Keep arithmetic on number literals exact
#'
#' R evaluates `1/2` to `0.5` before any Maxima expression is involved, so it
#' would reach Maxima as a float. This rewrites arithmetic on whole number
#' literals which R can't represent exactly (such as `1/2`, `2^-1` or
#' `1/(2 * 3)`) into the equivalent exact Maxima expression, embedded in the
#' body as a constant. Square roots, exponentials and logarithms of these (such
#' as `sqrt(2)`) are kept exact too. Arithmetic which R represents exactly (such as `2 * 3`) is
#' folded into a number.
#'
#' Only literals are rewritten. Values computed elsewhere, such as a variable
#' holding `1/2`, still reach Maxima as floats.
#'
#' @param x An R expression.
#'
#' @return An R expression.
#'
#' @noRd
exact_constants <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  x[-1] <- lapply(as.list(x[-1]), exact_constants)

  if (is_call(x, "(", n = 1L) && is_exact_constant(x[[2L]])) {
    return(x[[2L]])
  }
  # Irrational functions of whole numbers, such as `sqrt(2)`
  if (is_call(x, c("sqrt", "exp", "log"), n = 1L) && is_exact_constant(x[[2L]])) {
    if (is.numeric(x[[2L]]) && !is.finite(eval(x, baseenv()))) {
      return(x)
    }
    if (is.numeric(x[[2L]]) && is_whole(eval(x, baseenv()))) {
      return(eval(x, baseenv()))
    }
    return(maxima::mx_call(as.character(x[[1L]]), x[[2L]]))
  }
  if (!is_call(x, c("+", "-", "*", "/", "^"))) {
    return(x)
  }
  args <- as.list(x[-1])
  if (!all(vapply(args, is_exact_constant, logical(1)))) {
    return(x)
  }

  # Arithmetic which R represents exactly is folded into a number, so that it
  # can be part of larger constant expressions (as in `2^-1`). Arithmetic which
  # Maxima would reject (such as division by zero) is left to R.
  if (all(vapply(args, is.numeric, logical(1)))) {
    value <- eval(x, baseenv())
    if (is_whole(value)) {
      return(value)
    }
    if (!is.finite(value)) {
      return(x)
    }
  }
  do.call(as.character(x[[1L]]), lapply(args, maxima::as_mx_expr))
}

#' @noRd
is_exact_constant <- function(x) {
  inherits(x, "mx_expr") || (is.numeric(x) && length(x) == 1L && is_whole(x))
}

#' @noRd
is_whole <- function(x) {
  is.finite(x) && x == round(x) && abs(x) < 2^53
}

#' Capture an expression argument
#'
#' Arguments such as integration bounds and the right hand side of `solve()`
#' are bare R expressions, captured with [rlang::enquo()]. Every name in them is
#' a symbol, and R values are injected with `!!`. Strings are rejected, as they
#' were once parsed as R code.
#'
#' @param quo A quosure, from `enquo()`.
#' @param arg The name of the argument being captured, used in errors.
#'
#' @return The captured R expression.
#'
#' @noRd
capture_expr <- function(quo, arg) {
  expr <- quo_squash(quo)
  if (is.character(expr)) {
    hint <- if (is_string(expr) && !is.na(expr)) {
      sprintf("Did you mean `%s = %s`?", arg, expr)
    } else {
      sprintf("Write `%s` as a bare R expression, such as `%s = 2 * y`.", arg, arg)
    }
    abort(c(
      sprintf("`%s` must be an R expression, not a string.", arg),
      i = hint,
      i = "Names are symbols. Use `!!` to inject R values, as in `!!value`."
    ), call = NULL)
  }
  expr
}

#' Names in a captured expression which are symbols
#'
#' Every name in a captured expression is a symbol, except `pi` which is Maxima's `%pi`
#' unless it is an argument of the function. Function names aren't included.
#'
#' @param expr A captured expression, from `capture_expr()`.
#' @param nms The arguments of the function.
#'
#' @return A character vector of names.
#'
#' @noRd
expr_symbols <- function(expr, nms) {
  vars <- all.vars(expr)
  vars[!(vars %in% setdiff("pi", nms))]
}

#' Convert a captured expression to a Maxima expression
#'
#' Evaluates a captured expression with its symbols replaced by Maxima placeholders
#' (see `maxima_symbols()`). As in `as_maxima()`, literal fractions stay exact
#' and functions are masked by their Maxima equivalents, while other functions
#' are found in `env`. Expressions evaluating to `Inf` or `-Inf` become Maxima's
#' `inf` and `minf` when `infinite` is `TRUE`, and are an error otherwise.
#'
#' @param expr A captured expression, from `capture_expr()`.
#' @param nms The names in scope, which must include `expr_symbols(expr)`.
#' @param env The environment the expression was written in.
#' @param arg The name of the argument being converted, used in errors.
#' @param infinite Whether the expression may be infinite. Only bounds may be.
#'
#' @return An `mx_expr` object.
#'
#' @noRd
expr_to_maxima <- function(expr, nms, env, arg, infinite = arg %in% c("lower", "upper")) {
  vars <- expr_symbols(expr, nms)
  mask <- list2env(maxima_functions(), parent = env)
  res <- eval(exact_constants(expr), maxima_symbols(nms)[vars], mask)

  if (is.character(res)) {
    capture_expr(res, arg)
  }
  if (is.numeric(res) && length(res) == 1L && !is.na(res)) {
    if (is.infinite(res)) {
      if (!infinite) {
        abort(c(
          sprintf("`%s` must be finite.", arg),
          i = "`Inf` and `-Inf` are only meaningful as a bound of `integral()`."
        ), call = NULL)
      }
      return(maxima::mx_symbol(if (res > 0) "inf" else "minf"))
    }
    return(maxima::as_mx_expr(res))
  }
  if (!inherits(res, "mx_expr")) {
    abort(sprintf(
      "`%s` must evaluate to a single number or Maxima expression, not %s.",
      arg, describe_value(res)
    ), call = NULL)
  }
  if (has_infinity(expr) && !infinite) {
    abort(c(
      sprintf("`%s` must be finite.", arg),
      i = "`Inf` and `-Inf` are only meaningful as a bound of `integral()`."
    ), call = NULL)
  }
  if (has_infinity(expr)) {
    abort(sprintf(
      "`%s` can only be infinite as `Inf` or `-Inf` on its own, not as part of a larger expression.",
      arg
    ), call = NULL)
  }
  res
}

#' @noRd
has_infinity <- function(x) {
  if (is.numeric(x)) {
    return(any(is.infinite(x)))
  }
  is.call(x) && any(vapply(as.list(x[-1]), has_infinity, logical(1)))
}

#' @noRd
has_non_finite <- function(x) {
  if (is.numeric(x)) {
    return(any(!is.finite(x)))
  }
  is.call(x) && any(vapply(as.list(x[-1]), has_non_finite, logical(1)))
}

#' @noRd
describe_value <- function(x) {
  if (length(x) == 1L && is.atomic(x) && is.na(x)) {
    return("`NA`")
  }
  sprintf("a %s of length %i", class(x)[[1L]], length(x))
}
