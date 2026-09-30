f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
x <- c(0.5, 1, 2, 3)
y <- c(0, 1, 0.5, 4)

test_that("deriv() matches stats::deriv() for value, gradient and hessian", {
  sym <- deriv(f, c("x", "y"), hessian = TRUE)
  ref <- stats::deriv(~ log(2 * x + 1 + y), c("x", "y"), function.arg = TRUE, hessian = TRUE)

  res <- sym(x, y)
  expect_equal(res, ref(x, y))
  expect_equal(dim(attr(res, "gradient")), c(4L, 2L))
  expect_equal(dimnames(attr(res, "gradient")), list(NULL, c("x", "y")))
  expect_equal(dim(attr(res, "hessian")), c(4L, 2L, 2L))
  expect_equal(dimnames(attr(res, "hessian")), list(NULL, c("x", "y"), c("x", "y")))

  # Without the hessian
  expect_equal(deriv(f, c("x", "y"))(x, y), stats::deriv(~ log(2 * x + 1 + y), c("x", "y"), function.arg = TRUE)(x, y))
})

test_that("deriv() matches stats::deriv() for a subset of arguments, in any order", {
  expect_equal(
    deriv(f, "y", hessian = TRUE)(x, y),
    stats::deriv(~ log(2 * x + 1 + y), "y", function.arg = c("x", "y"), hessian = TRUE)(x, y)
  )
  expect_equal(
    deriv(f, c("y", "x"), hessian = TRUE)(x, y),
    stats::deriv(~ log(2 * x + 1 + y), c("y", "x"), function.arg = c("x", "y"), hessian = TRUE)(x, y)
  )
})

test_that("deriv() matches stats::deriv() on a range of functions", {
  g <- as_symbolic(function(a, b) exp(-a * b) * sin(a) + a^3 / b)
  a <- c(0.1, 0.7, 1.3)
  b <- c(2, 0.4, 1.1)
  expect_equal(
    deriv(g, hessian = TRUE)(a, b),
    stats::deriv(~ exp(-a * b) * sin(a) + a^3 / b, c("a", "b"), function.arg = TRUE, hessian = TRUE)(a, b)
  )

  # Derivatives which are constant are recycled to the length of the value.
  h <- as_symbolic(function(x, y) 3 * x + y^2)
  expect_equal(
    deriv(h, hessian = TRUE)(x, y),
    stats::deriv(~ 3 * x + y^2, c("x", "y"), function.arg = TRUE, hessian = TRUE)(x, y)
  )

  # Constant functions give a value of length one, as in stats.
  k <- as_symbolic(function(x) 5)
  expect_equal(deriv(k)(x), stats::deriv(~ 5, "x", function.arg = TRUE)(x))
})

test_that("deriv() matches stats::deriv() through special functions", {
  # The derivative of gamma() goes through digamma(), and trigamma().
  g <- as_symbolic(function(x, y) gamma(x^2 + y))
  expect_equal(
    deriv(g, hessian = TRUE)(x, y),
    stats::deriv(~ gamma(x^2 + y), c("x", "y"), function.arg = TRUE, hessian = TRUE)(x, y)
  )
})

test_that("deriv() differentiates functions stats::deriv() can't", {
  expect_error(stats::deriv(~ asinh(x * y), "x"), "not in the derivatives table")

  g <- as_symbolic(function(x, y) asinh(x * y))
  res <- deriv(g, hessian = TRUE)(x, y)
  expect_equal(as.vector(res), asinh(x * y))
  expect_equal(
    attr(res, "gradient"),
    array(c(y, x) / sqrt((x * y)^2 + 1), c(4L, 2L), list(NULL, c("x", "y")))
  )
  expect_equal(dim(attr(res, "hessian")), c(4L, 2L, 2L))
  expect_equal(dimnames(attr(res, "hessian")), list(NULL, c("x", "y"), c("x", "y")))
  expect_equal(attr(res, "hessian")[, "x", "x"], -x * y^3 / ((x * y)^2 + 1)^(3 / 2))
  expect_equal(attr(res, "hessian")[, "x", "y"], attr(res, "hessian")[, "y", "x"])
  expect_equal(attr(res, "hessian")[, "x", "y"], 1 / ((x * y)^2 + 1)^(3 / 2))

  # Numerical check of the gradient
  eps <- 1e-6
  expect_equal(
    attr(res, "gradient")[, "x"],
    (asinh((x + eps) * y) - asinh((x - eps) * y)) / (2 * eps),
    tolerance = 1e-6
  )
})

test_that("deriv() defaults to all arguments", {
  expect_equal(deriv(f), deriv(f, c("x", "y")))
  expect_equal(colnames(attr(deriv(f)(x, y), "gradient")), c("x", "y"))
})

test_that("deriv() returns a plain function or expression", {
  res <- deriv(f)
  expect_true(is.function(res))
  expect_false(is_symbolic(res))
  expect_equal(formals(res), formals(f))

  # Defaults of `expr` are kept
  g <- as_symbolic(function(x, y = 1) x * y)
  expect_equal(formals(deriv(g)), formals(g))
  expect_equal(deriv(g)(2), stats::deriv(~ x * y, c("x", "y"), function.arg = TRUE)(2, 1))

  # A character vector gives the arguments
  res <- deriv(f, "x", function.arg = c("y", "x"))
  expect_equal(names(formals(res)), c("y", "x"))
  expect_equal(res(y, x), deriv(f, "x")(x, y))

  # A function gives the arguments
  res <- deriv(f, "x", function.arg = function(x, y = 0) NULL)
  expect_equal(res(x), deriv(f, "x")(x, 0))

  # FALSE gives an expression, which can be evaluated
  for (fa in list(FALSE, NULL)) {
    res <- deriv(f, c("x", "y"), function.arg = fa)
    expect_true(is.expression(res))
    expect_equal(
      eval(res[[1L]], list(x = x, y = y)),
      eval(stats::deriv(~ log(2 * x + 1 + y), c("x", "y")), list(x = x, y = y))
    )
  }
})

test_that("deriv() validates its arguments", {
  expect_error(deriv(f, "z"), "`z` is not an argument")
  expect_error(deriv(f, c("x", "z", "w")), "`z`, `w` are not arguments")
  expect_error(deriv(f, quote(x)), "character vector")
  expect_error(deriv(f, character()), "character vector")
  expect_error(deriv(f, c("x", "x")), "duplicated")
  expect_error(deriv(f, "x", function.arg = 1), "function.arg")
  expect_error(deriv(f, "x", hessian = NA), "hessian")
  expect_error(deriv(f, "x", tag = 1), "tag")
  expect_error(deriv(as_symbolic(function() 1)), "character vector")
})

test_that("symbolic does not mask stats::integrate()", {
  # symbolic must remain usable as a low level dependency without disturbing
  # numerical integration
  expect_false("integrate" %in% getNamespaceExports("symbolic"))
  expect_equal(stats::integrate(function(x) x^2, 0, 1)$value, 1/3)
})
