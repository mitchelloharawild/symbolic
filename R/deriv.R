#' Symbolic derivatives with `deriv()`
#'
#' @description
#' A method for the [stats::deriv()] generic, which returns code computing the
#' value of a symbolic function along with its gradient (and optionally its
#' Hessian). Unlike [stats::deriv()], the derivatives are found by Maxima (with
#' [derivative()]), so any function Maxima can differentiate is supported,
#' such as `asinh()` and `abs()`.
#'
#' The result has the same form as the output of [stats::deriv()]: evaluating
#' it returns the value of `expr`, with a `"gradient"` attribute holding an
#' `n` by `length(namevec)` matrix of first derivatives, whose columns are
#' named by `namevec`. If `hessian = TRUE`, a `"hessian"` attribute holds an
#' `n` by `length(namevec)` by `length(namevec)` array of second derivatives.
#' The code is vectorised, so `n` is the length of the value.
#'
#' To find a derivative as a symbolic function, use [derivative()] instead.
#'
#' @param expr A symbolic function, as created by [as_symbolic()].
#' @param namevec A character vector of the names of arguments of `expr` to
#'   differentiate with respect to. Defaults to all arguments of `expr`.
#' @param function.arg Whether to return a function or an expression. If
#'   `TRUE` (the default), a function with the same arguments as `expr` is
#'   returned. If a character vector, a function with these arguments (without
#'   defaults) is returned. If a function, its arguments and environment are
#'   used. If `FALSE` (or `NULL`), an [expression()] is returned, which can be
#'   evaluated with [eval()] where the arguments of `expr` are defined.
#' @param tag Not used. It is accepted for compatibility with
#'   [stats::deriv()], which uses it to name common subexpressions. The code
#'   generated here doesn't use intermediate variables, since Maxima returns
#'   each derivative as a simplified expression.
#' @param hessian Whether to also compute the second derivatives, returned in
#'   the `"hessian"` attribute.
#' @param ... Not used.
#'
#' @return A function (or, if `function.arg = FALSE`, an expression) which
#'   computes the value of `expr` with `"gradient"` and optionally `"hessian"`
#'   attributes. The result is a plain function, not a symbolic function.
#'
#' @examples
#' f <- as_symbolic(function(x, y) log(2 * x + 1 + y))
#'
#' # Returns a function computing the value and gradient.
#' df <- deriv(f)
#' df
#' df(1:3, 0)
#'
#' # This matches stats::deriv() on the equivalent formula.
#' deriv(~ log(2 * x + 1 + y), c("x", "y"), function.arg = TRUE)(1:3, 0)
#'
#' # Differentiate with respect to some arguments, and include the Hessian.
#' deriv(f, "x", hessian = TRUE)(1, 0)
#'
#' # Return an expression, as stats::deriv() does by default.
#' deriv(f, "x", function.arg = FALSE)
#'
#' # Functions which stats::deriv() can't differentiate are supported.
#' g <- as_symbolic(function(x) asinh(x^2))
#' deriv(g)
#' try(deriv(~ asinh(x^2), "x"))
#'
#' @seealso [derivative()] for the derivative as a symbolic function, and
#'   [stats::deriv()].
#'
#' @name deriv.symbolic
#' @aliases symbolic-base-methods
#' @importFrom stats deriv
#' @export
deriv.symbolic <- function(expr, namevec = names(formals(expr)),
                           function.arg = TRUE, tag = ".expr",
                           hessian = FALSE, ...) {
  if (!is_symbolic(expr)) {
    abort("`expr` must be a symbolic function, as created by `as_symbolic()`.", call = NULL)
  }
  nms <- names(formals(expr))
  if (!is.character(namevec) || length(namevec) == 0L || anyNA(namevec) ||
      !all(nzchar(namevec))) {
    abort("`namevec` must be a character vector of argument names of `expr`.", call = NULL)
  }
  if (anyDuplicated(namevec)) {
    abort("`namevec` must not contain duplicated names.", call = NULL)
  }
  unknown <- setdiff(namevec, nms)
  if (length(unknown) > 0L) {
    abort(sprintf(
      "`namevec` must only contain arguments of `expr`.\n* %s %s, `expr` takes %s.",
      paste0("`", unknown, "`", collapse = ", "),
      if (length(unknown) == 1L) "is not an argument" else "are not arguments",
      paste0("`", nms, "`", collapse = ", ")
    ), call = NULL)
  }
  if (!is_string(tag)) {
    abort("`tag` must be a single string.", call = NULL)
  }
  if (!is.logical(hessian) || length(hessian) != 1L || is.na(hessian)) {
    abort("`hessian` must be `TRUE` or `FALSE`.", call = NULL)
  }

  code <- deriv_code(expr, namevec, hessian)

  if (is.null(function.arg) || isFALSE(function.arg)) {
    return(as.expression(list(code)))
  }
  if (isTRUE(function.arg)) {
    return(new_function(formals(expr), code, environment(expr)))
  }
  if (is.character(function.arg) && !anyNA(function.arg)) {
    args <- rep(list(quote(expr = )), length(function.arg))
    names(args) <- function.arg
    return(new_function(args, code, environment(expr)))
  }
  if (is.function(function.arg) && !is.primitive(function.arg)) {
    return(new_function(formals(function.arg), code, environment(function.arg)))
  }
  abort(
    "`function.arg` must be `TRUE`, `FALSE`, a character vector of argument names, or a function.",
    call = NULL
  )
}

#' Generate code computing a value with its gradient and Hessian
#'
#' Mirrors the structure of the code generated by [stats::deriv()], with each
#' derivative found symbolically by [derivative()].
#'
#' @return A `{` call.
#'
#' @noRd
deriv_code <- function(f, namevec, hessian) {
  k <- length(namevec)
  names_call <- call2("c", !!!namevec)

  grads <- lapply(namevec, function(nm) derivative(f, !!nm))
  names(grads) <- namevec

  code <- list(
    call2("<-", quote(.value), body(f)),
    call2("<-", quote(.grad), call2(
      "array", 0, call2("c", quote(length(.value)), k),
      call2("list", NULL, names_call)
    ))
  )
  if (hessian) {
    code <- c(code, list(call2("<-", quote(.hessian), call2(
      "array", 0, call2("c", quote(length(.value)), k, k),
      call2("list", NULL, names_call, names_call)
    ))))
  }

  for (i in seq_len(k)) {
    xi <- namevec[[i]]
    code <- c(code, list(bquote(.grad[, .(xi)] <- .(body(grads[[xi]])))))
    if (!hessian) next
    for (j in seq(i, k)) {
      xj <- namevec[[j]]
      d2 <- body(derivative(grads[[xi]], !!xj))
      if (i != j) {
        # Second derivatives are symmetric, so each is assigned to both halves.
        d2 <- bquote(.hessian[, .(xj), .(xi)] <- .(d2))
      }
      code <- c(code, list(bquote(.hessian[, .(xi), .(xj)] <- .(d2))))
    }
  }

  code <- c(
    code,
    list(quote(attr(.value, "gradient") <- .grad)),
    if (hessian) list(quote(attr(.value, "hessian") <- .hessian)),
    list(quote(.value))
  )
  call2("{", !!!code)
}
