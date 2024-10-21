#' @export
solve.symbolic <- function(a, b, target = names(a$alist)[[1]], ...) {
  yac <- as_yacas(a)

  b <- substitute(b)

  args <- yac$alist[setdiff(names(yac$alist), target)]
  # Add target to arguments if it is a new symbol
  if(is.symbol(b)) {
    b <- as_string(b)
    if(!(b %in% names(args))) {
      new_arg <- alist(x=)
      names(new_arg) <- b
      args <- c(new_arg, args)
    }
  }

  res <- new_yacas(
    solve(yac$yac, Ryacas::ysym(b), target),
    args
  )

  r_from_yacas(res)
}

#' @export
deriv.symbolic <- function(a, target = names(a$alist)[[1]], ...) {
  yac <- as_yacas(a)

  res <- new_yacas(
    deriv(yac$yac, target),
    yac$alist
  )

  r_from_yacas(res)
}

#' @inherit Ryacas::integrate
#' @export
integrate <- Ryacas::integrate

#' @exportS3Method Ryacas::integrate
#' @export
integrate.symbolic <- function(f, target = names(f$alist)[[1]], lower, upper, ...) {
  yac <- as_yacas(f)

  res <- if(missing(lower) && missing(upper)) {
    Ryacas::integrate(yac$yac, target, ...)
  } else {
    Ryacas::integrate(yac$yac, target, lower = lower, upper = upper, ...)
  }

  res <- new_yacas(
    res,
    yac$alist
  )

  r_from_yacas(res)
}
