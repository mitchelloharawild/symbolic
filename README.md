
<!-- README.md is generated from README.Rmd. Please edit that file -->

# symbolic

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/symbolic)](https://CRAN.R-project.org/package=symbolic)
<!-- badges: end -->

`symbolic` is an R package designed to provide symbolic algebra
capabilities natively in R using R functions.

It is highly experimental and dependent on external contributors to be a
fully fledged Computer Algebra System (CAS). It will be developed to the
extent needed for my upstream packages, specifically distributional
which requires many symbolic algebra tools not yet readily available in
R. Currently powered by [yacas](http://www.yacas.org/) via
[Ryacas](https://cran.r-project.org/package=Ryacas).

## Installation

You can install the development version of symbolic like so:

``` r
remotes::install_github("mitchelloharawild/symbolic")
```

## Usage

Prepare a function for symbolic transformations with `as_symbolic()`.

``` r
library(symbolic)
#> 
#> Attaching package: 'symbolic'
#> The following object is masked from 'package:stats':
#> 
#>     integrate
.f <- function(x, y) log(x + 1 + y)
.f
#> function(x, y) log(x + 1 + y)
.f_sym <- as_symbolic(.f)
.f_sym
#> {x, y}: log(x + 1 + y)
```

Internally, we use yacas for all the hard work.

``` r
.f_yac <- symbolic:::as_yacas(.f)
.f_yac
#> y: Ln(x+y+1)
symbolic:::r_from_yacas(.f_yac)
#> {x, y}: log(x + y + 1)
```

Typical base functions for math have `symbolic` methods which return
functions.

``` r
solve(.f_sym, y, "x")
#> {y}: exp(y) - 1 - y
deriv(.f_sym, "x")
#> {x, y}: 1/(x + y + 1)
integrate(.f_sym, "x")
#> {x, y}: (x + y + 1) * log(x + y + 1) - (x + y + 1)
```
