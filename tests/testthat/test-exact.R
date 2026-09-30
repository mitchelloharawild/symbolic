test_that("literal fractions reach Maxima exactly", {
  f <- as_symbolic(function(x) x^(1/2))

  expect_equal(deparse(body(derivative(f))), "1/(2 * sqrt(x))")
  expect_equal(deparse(body(integral(f))), "2 * x^(3/2)/3")
  expect_equal(body(integral(f, lower = 0, upper = 1)), quote(2/3))
})

test_that("literal arithmetic is folded before converting to fractions", {
  f <- as_symbolic(function(x) x^(2^-1) + 1/(2 * 3) * x - 1/2 * x^2)

  expect_equal(deparse(body(derivative(f))), "1/(2 * sqrt(x)) - x + 1/6")
  expect_equal(solve(as_symbolic(function(x) x^2 - 1/4), 0)(), cbind(-1/2, 1/2))
})

test_that("exact_constants() only rewrites literals", {
  # Whole number arithmetic is folded, but kept as R numbers
  expect_equal(exact_constants(quote(x + 2 * 3 - (4))), quote(x + 6 - 4))
  # Non-whole literals and division by zero are left to R
  expect_equal(exact_constants(quote(x^1.5)), quote(x^1.5))
  expect_equal(exact_constants(quote(x + 1/0)), quote(x + 1/0))
  expect_s3_class(exact_constants(quote(1/2)), "mx_expr")

  # Values computed elsewhere are not literals, and remain floats
  a <- 1/2
  expect_equal(deparse(body(derivative(as_symbolic(function(x) x^a)))), "0.5/x^0.5")
})

test_that("the original function is not modified", {
  f <- as_symbolic(function(x) x^(1/2))
  derivative(f)

  expect_equal(body(f), quote(x^(1/2)))
  expect_equal(f(4), 2)
})

test_that("literal irrational constants reach Maxima exactly", {
  expect_equal(deparse(body(integral(as_symbolic(function(x) sqrt(2) * pi * x)))), "pi * x^2/sqrt(2)")
  expect_equal(body(derivative(as_symbolic(function(x) exp(1)^x + log(1)))), quote(exp(x)))
})

test_that("sqrt(), exp() and log() of whole numbers are exact in bounds and `b`", {
  one <- as_symbolic(function(x) 1)
  expect_equal(body(integral(one, x, lower = 0, upper = sqrt(2))), quote(sqrt(2)))
  expect_equal(body(integral(one, x, lower = log(2), upper = exp(1))), quote(exp(1) - log(2)))

  sq <- as_symbolic(function(x) x^2)
  expect_equal(body(solve(sq, sqrt(2), x)), quote(cbind(-2^(1/4), 2^(1/4))))
  expect_equal(body(solve(sq, pi, x)), quote(cbind(-sqrt(pi), sqrt(pi))))
  expect_equal(deparse(body(solve(sq, 1/4, x))), "cbind(-1/2, 1/2)")
})

test_that("pi is exact in bodies unless it is an argument", {
  expect_equal(body(derivative(as_symbolic(function(x) pi * x))), quote(pi))
  expect_equal(body(derivative(as_symbolic(function(x, pi) pi * x))), quote(pi))
  expect_equal(body(derivative(as_symbolic(function(x, pi) pi * x^2), pi)), quote(x^2))
})

test_that("injected values arrive as decimals", {
  half <- 1/2
  # A decimal in a bound
  expect_equal(body(integral(as_symbolic(function(x) 1), x, lower = 0, upper = !!half)), 0.5)
})

test_that("Inf and -Inf are only meaningful as whole bounds", {
  f <- as_symbolic(function(x) exp(-x))
  expect_equal(body(integral(f, x, lower = 0, upper = Inf)), 1)
  expect_error(integral(f, x, lower = 0, upper = x + Inf), "on its own")

  sq <- as_symbolic(function(x) x^2)
  expect_error(solve(sq, Inf, x), "`b` must be finite", fixed = TRUE)
  expect_error(solve(sq, !!-Inf, x), "`b` must be finite", fixed = TRUE)
  expect_error(solve(sq, y + Inf, x), "`b` must be finite", fixed = TRUE)
})

test_that("Inf and -Inf in a function body are an error", {
  msg <- "its body contains an infinite or undefined value"
  # The sign of `Inf * x` depends on `x`, and it is undefined at `x = 0`
  expect_error(derivative(as_symbolic(function(x) Inf * x)), msg, fixed = TRUE)
  expect_error(integral(as_symbolic(function(x) Inf * x)), msg, fixed = TRUE)
  expect_error(solve(as_symbolic(function(x) x + Inf)), msg, fixed = TRUE)
  expect_error(derivative(as_symbolic(function(x) x - Inf)), msg, fixed = TRUE)
  expect_error(derivative(as_symbolic(function(x) Inf)), msg, fixed = TRUE)
  expect_error(derivative(as_symbolic(function(x) Inf)), "only meaningful as a bound", fixed = TRUE)

  # Infinities which Maxima would simplify away
  expect_error(derivative(as_symbolic(function(x) Inf * x * 0)), msg, fixed = TRUE)

  # Infinite values from elsewhere, and undefined results of them
  big <- Inf
  expect_error(derivative(as_symbolic(function(x) big * x)), msg, fixed = TRUE)
  expect_error(derivative(as_symbolic(function(x) x + big - big)), msg, fixed = TRUE)
})
