f <- as_symbolic(function(x, y) log(2 * x + 1 + y))

test_that("solve() solves for target against a value", {
  s <- solve(f, 0, x)

  expect_true(is_symbolic(s))
  # `target` is dropped, the remaining arguments are kept
  expect_equal(names(formals(s)), "y")
  expect_equal(body(s), quote(cbind(-y/2)))
  expect_equal(s(1), matrix(-0.5))
})

test_that("solve() round trips through the original function", {
  s <- solve(f, 0, x)
  expect_equal(f(s(1)[, 1], 1), 0)
})

test_that("solve() treats names in b as symbols, injecting values with !!", {
  rhs <- 0
  expect_equal(body(solve(f, !!rhs, x)), body(solve(f, 0, x)))

  # A name is a symbol even if a variable of that name exists
  expect_equal(names(formals(solve(f, rhs, x))), c("rhs", "y"))
})

test_that("solve() adds new symbols in b as arguments", {
  s <- solve(f, z, x)

  expect_equal(names(formals(s)), c("z", "y"))
  # s(z, y) is the x for which f(x, y) == z
  expect_equal(f(s(0, 0)[, 1], 0), 0)
  expect_equal(f(s(1.5, 2)[, 1], 2), 1.5)
})

test_that("solve() does not duplicate a symbol already in the arguments", {
  s <- solve(f, y, x)
  expect_equal(names(formals(s)), "y")
})

test_that("solve() returns multiple solutions as matrix columns", {
  s <- solve(as_symbolic(function(x) x^2), 4, x)

  expect_equal(s(), cbind(-2, 2))
})

test_that("solve() errors without explicit solutions", {
  expect_error(solve(as_symbolic(function(x) exp(x)), 0), "No solutions")
  expect_error(solve(as_symbolic(function(x) x - exp(-x)), 0), "solved for explicitly")
  # Maxima may return the equation unsolved, rather than `x == value`
  expect_error(solve(as_symbolic(function(x) x^x), 2), "solved for explicitly")
  expect_error(solve(as_symbolic(function(x) abs(x))), "solved for explicitly")
})

test_that("solve() defaults to the first argument", {
  expect_equal(body(solve(f, 0)), body(solve(f, 0, x)))
})

test_that("solve() accepts expressions in b", {
  s <- solve(f, 2 * y + a, x)

  expect_equal(names(formals(s)), c("a", "y"))
  expect_equal(f(s(0.5, 1)[, 1], 1), 2 * 1 + 0.5)
  # Literal fractions stay exact
  expect_equal(body(solve(as_symbolic(function(x) 2 * x), 1/2, x)), quote(cbind(1/4)))
})

test_that("solve() can solve for fixed points", {
  # b may refer to the target itself
  s <- solve(as_symbolic(function(x) x^2), x, x)
  expect_equal(s(), cbind(0, 1))
})

test_that("solve() validates its arguments", {
  expect_error(solve(f, 0, z), "must be an argument")
  expect_error(solve(f, "z", x), "not a string")
  expect_error(solve(f, c(1, 2), x), "single number")
  expect_error(solve(f, 0, x + 1), "name of an argument")
})

test_that("solve() accepts targets as strings and injected names", {
  var <- "x"
  expect_equal(solve(f, 0, !!var), solve(f, 0, x))
  expect_equal(solve(f, 0, "x"), solve(f, 0, x))
})

test_that("solve() always returns a matrix, with a column per solution", {
  s <- solve(as_symbolic(function(x) x^2), y, x)
  expect_equal(body(s), quote(cbind(-sqrt(y), sqrt(y))))

  # One row per element of vectorised input
  expect_equal(s(c(1, 4, 9)), cbind(c(-1, -2, -3), c(1, 2, 3)))

  # Including when there is a single solution
  s <- solve(f, 0, x)
  expect_equal(s(c(1, 3)), matrix(c(-0.5, -1.5)))
})

test_that("solve() without b returns the inverse", {
  inv <- solve(as_symbolic(function(x) exp(x)))
  expect_true(is_symbolic(inv))
  # The value of `a` takes the name of `target`
  expect_equal(names(formals(inv)), "x")
  expect_equal(body(inv), quote(cbind(log(x))))
  expect_equal(inv(c(1, exp(2))), matrix(c(0, 2)))
})

test_that("solve() inverts functions of several arguments", {
  # The value takes the position of `target`, other arguments are kept
  inv <- solve(f, target = x)
  expect_equal(names(formals(inv)), c("x", "y"))
  expect_equal(f(inv(2, 1)[, 1], 1), 2)

  inv <- solve(f, target = y)
  expect_equal(names(formals(inv)), c("x", "y"))
  expect_equal(f(1, inv(1, 2)[, 1]), 2)

  # The inverse of x^3 + y has three (complex) cube roots
  inv <- solve(as_symbolic(function(x, y) x^3 + y), target = x)
  expect_equal(names(formals(inv)), c("x", "y"))
  expect_equal(dim(inv(9, 1)), c(1L, 3L))
  expect_equal(Re(inv(9, 1)[, 3]), 2)
})

test_that("solve() returns each branch of an inverse as a column", {
  inv <- solve(as_symbolic(function(x) x^2))
  expect_equal(body(inv), quote(cbind(-sqrt(x), sqrt(x))))
  expect_equal(inv(c(4, 9)), cbind(c(-2, -3), c(2, 3)))
})

test_that("solve() drops the default of target in an inverse", {
  inv <- solve(as_symbolic(function(x = 2, y = 1) x + y))
  expect_identical(formals(inv)$x, quote(expr = ))
  expect_equal(formals(inv)$y, 1)
})

test_that("solve() reports Maxima's questions about an inverse with R names", {
  # Maxima asks about the value of `a`, which takes the name of `target`
  expect_error(
    solve(as_symbolic(function(x) x^(2/3))),
    'asked: "Is x positive or zero?"',
    fixed = TRUE
  )
})
