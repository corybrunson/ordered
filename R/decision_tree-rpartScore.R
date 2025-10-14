#' A wrapper for `rpartScore`
#'
#' A wrapper is used because the model interface requires the response variable
#' to be numeric rather than ordered or factor; the wrapper edits the input
#' `data` accordingly.
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("rpartScore")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # fit wrapper
#' ( fit_orig <- rpartScore::rpartScore(
#'   formula = Sat ~ Infl + Type + Cont,
#'   data = transform(house_data, Sat = as.integer(Sat))
#' ) )
#' ( fit_wrap <- rpartScore_wrapper(
#'   formula = Sat ~ Infl + Type + Cont,
#'   data = house_data
#' ) )
#'
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
