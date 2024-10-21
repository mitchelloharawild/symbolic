r_from_yacas <- function(x) {
  stopifnot(inherits(x, "yacas"))
  body <- Ryacas::as_r(x$yac)[[1]]

  # Handle multiple outputs as matrix
  body <- if(call_name(body) == "c") {
    body[-1]
  } else {
    list(body)
  }

  # Remove "x =="
  body <- lapply(body, function(x) {
    if(call_name(x) == "==") x[[3L]] else x
  })

  # Combine multiple results as matrix columns
  body <- if(length(body) > 1) {
    call2("cbind", !!!body)
  } else {
    body[[1L]]
  }

  # Create function
  as_symbolic(new_function(x$alist, body))
}
