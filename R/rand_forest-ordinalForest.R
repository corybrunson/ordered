#' A wrapper for `ordinalForest`
#'
#' A wrapper is needed since they have a non-standard model interface that
#' required the data set and the column name (character string) for the
#' outcome.
#' @param x The predictor data.
#' @param y The outcome factor.
#' @param ... Arguments to pass to the underlying model function.
#' @export
#' @keywords internal
ordinalForest_wrapper <- function(x, y, ...) {
  rlang::check_installed("ordinalForest")
  # append response variable as column to predictor matrix
  x$.outcome <- y
  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "ordfor", .ns = "ordinalForest",
    depvar = ".outcome", data = expr(x), ...
  )
  rlang::eval_tidy(cl)
}
