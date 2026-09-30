f <- as_symbolic(function(x, y) log(2 * x + 1 + y))

test_that("integral() computes indefinite integrals", {
  i <- integral(f, x)

  expect_true(is_symbolic(i))
  # Indefinite integrals keep every argument
  expect_equal(names(formals(i)), c("x", "y"))
})

test_that("integral() inverts derivative()", {
  expect_equal(integral(derivative(f, x), x)(1, 0), f(1, 0))
})

# Definite integrals of `f` need the sign of `y`, which Maxima would ask for
g <- as_symbolic(function(x, y) x^2 + x * y)

test_that("integral() computes definite integrals", {
  i <- integral(g, x, lower = 0, upper = 1)

  # The variable of integration is dropped
  expect_equal(names(formals(i)), "y")
  expect_equal(i(2), stats::integrate(function(x) x^2 + 2 * x, 0, 1)$value)
})

test_that("integral() evaluates bounds rather than deparsing them", {
  # Regression: Ryacas deparses `lower`/`upper`, so variables leaked their names
  lo <- 0
  hi <- 1

  expect_equal(
    integral(g, x, lower = !!lo, upper = !!hi)(2),
    integral(g, x, lower = 0, upper = 1)(2)
  )
})

test_that("integral() bounds treat names as symbols, even if a variable exists", {
  lo <- 0
  i <- integral(g, x, lower = lo, upper = 1)

  # `lo` is a new argument, not the value of the variable `lo`
  expect_equal(names(formals(i)), c("lo", "y"))
  expect_equal(i(0.5, 2), stats::integrate(function(x) x^2 + 2 * x, 0.5, 1)$value)
})

test_that("integral() supports infinite bounds", {
  expect_equal(integral(as_symbolic(function(x) exp(-x)), x, lower = 0, upper = Inf)(), 1)
  expect_equal(integral(as_symbolic(function(x) exp(-x^2)), x, lower = -Inf, upper = Inf)(), sqrt(pi))

  # Injected infinite values
  lo <- -Inf
  expect_equal(integral(as_symbolic(function(x) exp(-x^2)), x, lower = !!lo, upper = !!Inf)(), sqrt(pi))
  expect_equal(integral(as_symbolic(function(x) exp(x)), x, lower = -!!Inf, upper = 0)(), 1)

  # Infinity can't be part of a larger expression
  expect_error(integral(g, x, lower = 0, upper = y + Inf), "on its own")
})

test_that("integral() bounds can be positional", {
  expect_equal(names(formals(integral(g, x, 0, q))), c("q", "y"))
})

test_that("integral() adds new symbols in bounds as arguments", {
  expect_equal(names(formals(integral(g, x, lower = 0, upper = b))), c("b", "y"))

  # In order of first appearance, `lower` then `upper`, without duplicates
  i <- integral(g, x, lower = a + b, upper = c * a + b * y)
  expect_equal(names(formals(i)), c("a", "b", "c", "y"))
  expect_equal(
    i(1, 2, 3, 4),
    stats::integrate(function(x) x^2 + 4 * x, 1 + 2, 3 * 1 + 2 * 4)$value
  )
})

test_that("integral() bounds can refer to arguments of `f`", {
  i <- integral(g, x, lower = 0, upper = 2 * y)

  expect_equal(names(formals(i)), "y")
  expect_equal(i(1), stats::integrate(function(x) x^2 + x, 0, 2)$value)
  expect_equal(names(formals(integral(g, x, 0, y))), "y")
})

test_that("integral() bounds resolve functions and constants", {
  i <- integral(g, x, lower = 0, upper = 2 * sqrt(y))
  expect_equal(names(formals(i)), "y")
  expect_equal(i(4), stats::integrate(function(x) x^2 + 4 * x, 0, 4)$value)

  # `pi` is exact, as in the body of a function
  i <- integral(as_symbolic(function(x) cos(x)), x, lower = 0, upper = pi / 2)
  expect_equal(body(i), 1)
})

test_that("integral() keeps fractions in bounds exact", {
  i <- integral(as_symbolic(function(x) x), x, lower = 0, upper = 1/2)
  expect_identical(body(i), quote(1/8))

  # Fractions computed elsewhere are injected as floats
  half <- 1/2
  expect_equal(integral(as_symbolic(function(x) x), x, lower = 0, upper = !!half)(), 1/8)
})

test_that("integral() accepts injected Maxima expressions as bounds", {
  i <- integral(g, x, lower = 0, upper = !!maxima::mx_call("sqrt", 2L))
  expect_equal(names(formals(i)), "y")
  expect_equal(i(1), stats::integrate(function(x) x^2 + x, 0, sqrt(2))$value)
})

test_that("integral() bounds can mention the variable of integration", {
  dens <- as_symbolic(function(x, rate) rate * exp(-rate * x))
  cdf <- integral(dens, x, lower = 0, upper = x)

  # The result is a function of `x`, like a CDF
  expect_equal(names(formals(cdf)), c("x", "rate"))
  expect_equal(cdf(1.3, 2), 1 - exp(-2 * 1.3))
  expect_equal(cdf(c(0, 0.5, 4), 0.7), 1 - exp(-0.7 * c(0, 0.5, 4)))

  # Both bounds, and alongside new symbols
  i <- integral(g, x, lower = x, upper = 2 * x + b)
  expect_equal(names(formals(i)), c("b", "x", "y"))
  expect_equal(i(1, 2, 3), stats::integrate(function(t) t^2 + 3 * t, 2, 5)$value)
})

test_that("integral() computes a CDF from a density with an injected bound", {
  dens <- as_symbolic(function(x, rate) rate * exp(-rate * x))
  support_min <- 0
  p <- integral(dens, x, lower = !!support_min, upper = q)

  expect_equal(names(formals(p)), c("q", "rate"))
  expect_equal(p(c(0.2, 1.3, 5), 2), 1 - exp(-2 * c(0.2, 1.3, 5)))
})

test_that("integral() results are simplified", {
  # Maxima returns `rate * (1/rate - exp(-rate * q)/rate)` unsimplified
  dens <- as_symbolic(function(x, rate) rate * exp(-rate * x))
  expect_identical(body(integral(dens, x, lower = 0, upper = q)), quote(1 - exp(-rate * q)))

  # Compact forms aren't expanded
  i <- integral(g, x, lower = 0, upper = b)
  expect_identical(body(i), quote(b^2 * (3 * y + 2 * b)/6))
  expect_identical(body(integral(g, x)), quote(x^3/3 + x^2 * y/2))
})

test_that("integral() rejects string bounds", {
  expect_error(integral(g, x, lower = 0, upper = "b"), "not a string")
  expect_error(integral(g, x, lower = 0, upper = "b"), "upper = b", fixed = TRUE)
  expect_error(integral(g, x, lower = "2 * y", upper = 1), "lower = 2 * y", fixed = TRUE)
  # Injected strings too
  lo <- "a"
  expect_error(integral(g, x, lower = !!lo, upper = 1), "not a string")
})

test_that("integral() errors when no closed form is found", {
  expect_error(integral(as_symbolic(function(x) x^x), x), "closed form")
})

test_that("integral() requires both bounds", {
  expect_error(integral(f, x, lower = 0), "both be supplied")
  expect_error(integral(f, x, upper = 1), "both be supplied")
})

test_that("integral() validates bounds and target", {
  expect_error(integral(f, z), "must be an argument")
  expect_error(integral(f, x, lower = 0, upper = c(1, 2)), "single number")
  expect_error(integral(f, x, lower = 0, upper = NA), "single number")
  expect_error(integral(f, x, lower = 0, upper = !!c(1, 2)), "single number")
  expect_error(integral(f, x, lower = 0, upper = !!TRUE), "single number")
})

test_that("integral() reports questions Maxima asks about arguments", {
  # Maxima needs the sign of `y`, and must refer to it by its R name
  expect_error(
    integral(f, x, lower = 0, upper = 1),
    'asked: "Is y positive, negative or zero?"',
    fixed = TRUE
  )
  # Questions about new symbols in bounds use their R names too
  expect_error(
    integral(as_symbolic(function(x) 1 / x), x, lower = 1, upper = Upper),
    'asked: "Is Upper',
    fixed = TRUE
  )
  # The embedded Maxima remains usable afterwards
  expect_equal(integral(g, x, lower = 0, upper = 1)(0), 1/3)
})

test_that("integral() hints that `target` comes before the bounds", {
  g <- as_symbolic(function(x, y) x^2 + x * y)
  expect_error(integral(g, 0, 1), "Did you mean `integral(f, x, 0, 1)`?", fixed = TRUE)
  expect_error(integral(g, -1, 1), "Did you mean `integral(f, x, -1, 1)`?", fixed = TRUE)
  expect_error(integral(g, pi / 2, 1), "Did you mean `integral(f, x, pi/2, 1)`?", fixed = TRUE)

  # No hint when `target` is named, or isn't bound-like
  expect_no_error(tryCatch(integral(g, target = 0), error = function(e) {
    expect_false(grepl("Did you mean", conditionMessage(e)))
  }))
  expect_error(integral(g, z), "must be an argument")
})
