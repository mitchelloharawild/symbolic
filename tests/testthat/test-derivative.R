f <- as_symbolic(function(x, y) log(2 * x + 1 + y))

test_that("derivative() differentiates with respect to target", {
  expect_equal(body(derivative(f, x)), quote(2/(2 * x + y + 1)))
  expect_equal(body(derivative(f, y)), quote(1/(2 * x + y + 1)))
})

test_that("derivative() returns an evaluable symbolic function", {
  d <- derivative(f, x)

  expect_true(is_symbolic(d))
  expect_equal(formals(d), formals(f))
  expect_equal(d(1, 0), 2 / 3)
})

test_that("derivative() defaults to the first argument", {
  expect_equal(body(derivative(f)), body(derivative(f, x)))
})

test_that("derivative() handles results which are not calls", {
  # Regression: constants and symbols are not calls, and once errored
  expect_equal(body(derivative(as_symbolic(function(x) 2 * x), x)), quote(2))
  expect_equal(body(derivative(as_symbolic(function(x) 5), x)), quote(0))
  expect_equal(body(derivative(as_symbolic(function(x, y) x * y), x)), quote(y))
})

test_that("derivative() results are simplified", {
  # Maxima returns `-x/(x + 1)^2 + 1/(x + 1)` unsimplified
  expect_identical(body(derivative(as_symbolic(function(x) x / (1 + x)))), quote(1/(x + 1)^2))
  # Compact forms aren't expanded
  expect_identical(body(derivative(as_symbolic(function(x) (x + 1)^10))), quote(10 * (x + 1)^9))
})

test_that("derivative() can be applied repeatedly", {
  expect_equal(body(derivative(derivative(as_symbolic(function(x) x^3), x), x)), quote(6 * x))
})

test_that("derivative() validates target", {
  expect_error(derivative(f, z), "must be an argument")
  expect_error(derivative(f, 1), "name of an argument")
  expect_error(derivative(f, c("x", "y")), "name of an argument")
  expect_error(derivative(f, !!c("x", "y")), "name of an argument")
})

test_that("derivative() results can be manipulated again", {
  # Maxima's psi[n](x) is returned as R's digamma(), trigamma() and psigamma()
  g <- as_symbolic(function(x) gamma(x))
  d <- derivative(derivative(g))

  expect_equal(deparse(body(derivative(g))), "digamma(x) * gamma(x)")
  expect_equal(d(2), (digamma(2)^2 + trigamma(2)) * gamma(2))
  expect_match(deparse1(body(derivative(d))), "psigamma(x, 2)", fixed = TRUE)

  # Maxima's erf() is returned via pnorm()
  i <- integral(as_symbolic(function(x) exp(-x^2)))
  expect_equal(body(derivative(i)), quote(exp(-x^2)))
})

test_that("derivative() understands pnorm()", {
  d <- derivative(as_symbolic(function(x, mu, s) pnorm(x, mu, s)))
  expect_equal(d(1, 0.5, 2), dnorm(1, 0.5, 2))

  d <- derivative(as_symbolic(function(x) pnorm(x, lower.tail = FALSE)))
  expect_equal(d(1), -dnorm(1))
})

test_that("derivative() handles abs()", {
  d <- derivative(as_symbolic(function(x) abs(x)))
  expect_equal(d(-2), -1)
})

test_that("derivative() understands special functions R doesn't dispatch", {
  # Compares with central differences of `f` in its first argument
  expect_derivative <- function(f, x, y = NULL) {
    call_f <- function(x) if (is.null(y)) f(x) else f(x, y)
    num <- (call_f(x + 1e-6) - call_f(x - 1e-6)) / 2e-6
    d <- derivative(as_symbolic(f))
    expect_equal(if (is.null(y)) d(x) else d(x, y), num, tolerance = 1e-6)
  }

  expect_match(deparse1(body(derivative(as_symbolic(function(a, b) beta(a, b))))), "beta(a, b)", fixed = TRUE)
  expect_match(deparse1(body(derivative(as_symbolic(function(n, k) choose(n, k))))), "choose(n, k)", fixed = TRUE)
  expect_derivative(function(a, b) beta(a, b), 1.5, 2.5)
  expect_derivative(function(a, b) lbeta(a, b), 1.5, 2.5)
  expect_derivative(function(n, k) choose(n, k), 5.5, 2)
  expect_derivative(function(n, k) lchoose(n, k), 5.5, 2)
  expect_derivative(function(x) lfactorial(x), 2.5)
  expect_derivative(function(x) lgamma(x), 2.5)
  expect_derivative(function(y, x) atan2(y, x), 1.5, -2)
  expect_derivative(function(x, nu) besselJ(x, nu), 1.5, 2)
  expect_derivative(function(x, nu) besselY(x, nu), 1.5, 2)
  expect_derivative(function(x, nu) besselI(x, nu), 1.5, 2)
  expect_derivative(function(x, nu) besselK(x, nu), 1.5, 2)
  expect_derivative(function(x) besselI(x, 1, expon.scaled = TRUE), 1.5)
  expect_derivative(function(x) besselK(x, 1, expon.scaled = TRUE), 1.5)
})

test_that("derivative() understands math functions Maxima names differently", {
  expect_equal(body(derivative(as_symbolic(function(x) log(x, 2)))), quote(1/(log(2) * x)))
  expect_equal(body(derivative(as_symbolic(function(x) log2(x)))), quote(1/(log(2) * x)))
  expect_equal(body(derivative(as_symbolic(function(x) log10(x)))), quote(1/(log(10) * x)))
  expect_equal(body(derivative(as_symbolic(function(x, b) log(x, base = b)))), quote(1/(x * log(b))))
  expect_equal(body(derivative(as_symbolic(function(x) log1p(x)))), quote(1/(x + 1)))
  expect_equal(body(derivative(as_symbolic(function(x) expm1(x)))), quote(exp(x)))
  expect_equal(body(derivative(as_symbolic(function(x) lgamma(x)))), quote(digamma(x)))
  expect_equal(body(derivative(as_symbolic(function(x) sinpi(x)))), quote(pi * cos(pi * x)))
  expect_equal(derivative(as_symbolic(function(x) tanpi(x)))(0.3), pi / cospi(0.3)^2)

  # Constants are still evaluated by R
  expect_equal(body(derivative(as_symbolic(function(x) x * log(8, 2)))), 3)

  # Maxima's sign() and trunc() have other meanings, and none has a derivative
  expect_error(derivative(as_symbolic(function(x) x * sign(x))), "closed form")
  expect_error(derivative(as_symbolic(function(x) x * trunc(x))), "closed form")
  expect_error(derivative(as_symbolic(function(x) x * round(x, 1))), "closed form")
  expect_equal(body(integral(as_symbolic(function(x) sign(x)))), quote(abs(x)))
})

test_that("derivative() accepts targets as bare names, strings and injected names", {
  var <- "y"
  expect_equal(derivative(f, !!var), derivative(f, y))
  expect_equal(derivative(f, "y"), derivative(f, y))
  expect_equal(derivative(f, !!as.name(var)), derivative(f, y))

  # A variable named like an argument is not evaluated
  x <- "y"
  expect_equal(derivative(f, x), derivative(f, !!quote(x)))
})
