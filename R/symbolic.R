#' Convert a Function to Symbolic Class
#'
#' This function takes a regular function and converts it into a symbolic function by assigning it a class of "symbolic".
#' It is intended to prepare functions for symbolic transformations using the `symbolic` package.
#'
#' @param .f A function to be converted into a symbolic function.
#'
#' @return A function with an added "symbolic" class, allowing it to be used in symbolic transformations.
#'
#' @examples
#' .f <- function(x) x^2
#' symbolic_f <- as_symbolic(.f)
#' class(symbolic_f)  # Returns "symbolic"
#'
#' @export
as_symbolic <- function(.f) {
  structure(.f, class = "symbolic")
}

#' @export
print.symbolic <- function(x, ...) {
  cat(sprintf(
    "{%s}: %s",
    paste0(names(formals(x)), collapse = ", "),
    deparse(body(x))
  ))
}
