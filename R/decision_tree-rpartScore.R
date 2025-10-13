#' A wrapper for `rpartScore`
#'
#' A wrapper is used because the model interface requires the response variable
#' to be numeric rather than ordered or factor; the wrapper edits the input
#' `data` accordingly.
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param ... Additional arguments to pass.
#' @export
#' @keywords internal
rpartScore_wrapper <- function(formula, data, ...) {
  rlang::check_installed("rpartScore")
  # convert response variable in `data` from ordinal to integer
  lhs <- rlang::f_lhs(formula)
  data[[lhs]] <- as.integer(data[[lhs]])
  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "rpartScore", .ns = "rpartScore",
    formula = expr(formula), data = expr(data), ...
  )
  rlang::eval_tidy(cl)
}
