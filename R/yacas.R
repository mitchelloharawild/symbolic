
as_yacas <- function(.f, args = formals(.f)) {
  yacas_symbols <- lapply(names(args), Ryacas::ysym)
  names(yacas_symbols) <- names(args)
  new_yacas(do.call(.f, yacas_symbols), args)
}

new_yacas <- function(yac, args) {
  structure(
    list(
      yac = yac,
      alist = args
    ),
    class = "yacas"
  )
}

#' @export
print.yacas <- function(x, ...) {
  print(x$yac)
}
