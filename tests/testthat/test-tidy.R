expect_tidy <- function(x, expected) {
  expect_equal(deparse(tidy_expr(x)), expected)
}

test_that("tidy_expr() writes Maxima results as idiomatic R code", {
  # Maxima normalises expressions into sums and products of powers
  expect_tidy(quote(-1 + x * y^-1), "x/y - 1")
  expect_tidy(quote(2 * (1 + 2 * x + y)^-1), "2/(2 * x + y + 1)")
  expect_tidy(quote(-1/2 * y), "-y/2")
  expect_tidy(quote(-1 * x * y), "-x * y")
  expect_tidy(quote(-2 * x + y), "-2 * x + y")
  expect_tidy(quote(y + -2 * x), "y - 2 * x")
  expect_tidy(quote(1 + -1 * exp(1)^(-x)), "1 - exp(-x)")
  expect_tidy(quote(-1 * (x + y)^2), "-(x + y)^2")
  expect_tidy(quote(3 * x^-1 * y^-2), "3/(x * y^2)")
  expect_tidy(quote(x^(-1/2)), "1/sqrt(x)")
})

test_that("tidy_expr() translates functions R lacks", {
  expect_tidy(quote(exp(1)^x), "exp(x)")
  expect_tidy(quote(x^(1/2)), "sqrt(x)")
  erf <- eval(call("function", as.pairlist(alist(x = )), tidy_expr(quote(erf(x)))))
  expect_equal(erf(0.5), 0.5204998778, tolerance = 1e-9)
})

test_that("tidy_expr() leaves atoms and other calls alone", {
  expect_tidy(quote(x), "x")
  expect_equal(tidy_expr(2), 2)
  expect_tidy(quote(log(x + 1)), "log(x + 1)")
})

test_that("tidy_expr() translates Maxima's polygamma functions", {
  expect_tidy(quote(psi[0](x)), "digamma(x)")
  expect_tidy(quote(psi[1](x)), "trigamma(x)")
  expect_tidy(quote(psi[2](x + 1)), "psigamma(x + 1, 2)")
})

test_that("tidy_expr() translates Maxima's special functions", {
  expect_tidy(quote(binomial(n, k)), "choose(n, k)")
  expect_tidy(quote(beta(a, b)), "beta(a, b)")
  expect_tidy(quote(bessel_j(nu, x)), "besselJ(x, nu)")
  expect_tidy(quote(bessel_y(nu, x)), "besselY(x, nu)")
  expect_tidy(quote(bessel_i(1 + nu, x)), "besselI(x, nu + 1)")
  expect_tidy(quote(bessel_k(nu, x)), "besselK(x, nu)")
  expect_tidy(quote(signum(x)), "sign(x)")
  expect_tidy(quote(truncate(x)), "trunc(x)")
  expect_tidy(quote(sec(x)^2), "(1/cos(x))^2")
  expect_tidy(quote(csc(x)), "1/sin(x)")
  expect_tidy(quote(coth(2 * x)), "1/tanh(2 * x)")
})

test_that("special functions survive a round trip through Maxima", {
  fns <- list(
    function(a, b) beta(a, b),
    function(n, k) choose(n, k),
    function(y, x) atan2(y, x),
    function(x, nu) besselJ(x, nu),
    function(x, nu) besselK(x, nu),
    function(x) sign(x),
    function(x) trunc(x)
  )
  for (f in fns) {
    expect_equal(body(r_from_maxima(as_maxima(f), names(formals(f)), formals(f), globalenv())), body(f))
  }
})

test_that("unevaluated Maxima results are reported", {
  x <- maxima::mx_symbol("x")
  expect_error(
    r_from_maxima(maxima::mx_call("diff", maxima::mx_call("f", x), x), "x", alist(x = ), globalenv()),
    "closed form"
  )
})
