test_that("as_symbolic() marks functions as symbolic", {
  f <- as_symbolic(function(x) x^2)

  expect_s3_class(f, "symbolic")
  expect_true(is_symbolic(f))
  expect_false(is_symbolic(function(x) x^2))

  # Symbolic functions remain ordinary functions
  expect_type(f, "closure")
  expect_equal(f(3), 9)
})

test_that("as_symbolic() preserves existing classes and is idempotent", {
  f <- structure(function(x) x^2, class = "myclass")
  sym <- as_symbolic(f)

  expect_s3_class(sym, c("symbolic", "myclass"), exact = TRUE)
  expect_identical(as_symbolic(sym), sym)
})

test_that("as_symbolic() rejects invalid input", {
  expect_error(as_symbolic("x"), "must be a function")
  expect_error(as_symbolic(1), "must be a function")
  expect_error(as_symbolic(sum), "must be a closure")
  expect_error(as_symbolic(function(x, ...) x), "must not use")
})

test_that("print.symbolic() shows arguments and body", {
  f <- as_symbolic(function(x, y) log(2 * x + 1 + y))

  expect_output(print(f), "{x, y} -> log(2 * x + 1 + y)", fixed = TRUE)
  expect_invisible(print(f))
})

test_that("print.symbolic() does not truncate long bodies", {
  f <- as_symbolic(function(x) x^2 + x^3 + x^4 + x^5 + x^6 + x^7 + x^8 + x^9 + x^10 + x^11)

  # deparse() splits long expressions over several lines
  expect_output(print(f), "x^11", fixed = TRUE)
})

test_that("argument names are preserved through Maxima", {
  # Maxima is case-insensitive, and reserves names such as `inf`
  f <- as_symbolic(function(N, n, inf) N * n * inf^2)

  expect_equal(body(derivative(f, inf)), quote(2 * N * n * inf))
  expect_equal(body(derivative(f, N)), quote(n * inf^2))
})

test_that("as_symbolic() takes the function as `f`", {
  expect_true(is_symbolic(as_symbolic(f = function(x) x^2)))
  expect_error(as_symbolic(f = 1), "`f` must be a function", fixed = TRUE)
})

test_that("names in a body which aren't arguments are evaluated as R code", {
  a <- 2
  f <- as_symbolic(function(x) a * x^2)
  expect_equal(body(derivative(f)), quote(4 * x))

  # The value is fixed when the function is transformed
  d <- derivative(f)
  a <- 3
  expect_equal(d(1), 4)
})

test_that("undefined names in a body suggest making them arguments", {
  f <- as_symbolic(function(x) k * x^2)
  expect_error(derivative(f), "`k` in the body of `f` isn't an argument", fixed = TRUE)
  expect_error(derivative(f), "evaluated as R code", fixed = TRUE)
  expect_error(derivative(f), "`function(x, k) k * x^2`", fixed = TRUE)
  expect_error(integral(f, x, 0, 1), "`function(x, k) k * x^2`", fixed = TRUE)

  # Other errors are left unchanged
  expect_error(derivative(as_symbolic(function(x) stop("boom"))), "^boom$")
  helper <- function(z) undefined_helper_value + z
  expect_error(
    derivative(as_symbolic(function(x) helper(x))),
    "^object 'undefined_helper_value' not found$"
  )
})

test_that("results of solve() with one solution can be manipulated again", {
  f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
  s <- solve(f, 0, x)
  d <- derivative(s)
  expect_equal(names(formals(d)), "y")
  expect_equal(deparse(body(d)), "-1/2")
  expect_equal(body(derivative(solve(as_symbolic(function(x) exp(x)), y, x))), quote(1/y))
})

test_that("results of solve() with several solutions can't be manipulated", {
  s <- solve(as_symbolic(function(x) x^2), y, x)
  expect_error(derivative(s), "returns 2 solutions", fixed = TRUE)
  expect_error(integral(s, y), "can't be manipulated as a single expression", fixed = TRUE)
})
