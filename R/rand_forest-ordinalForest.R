#' A wrapper for `ordinalForest`
#'
#' A wrapper is needed since they have a non-standard model interface that
#' required the data set and the column name (character string) for the outcome.
#' @param x The predictor data.
#' @param y The outcome factor.
#' @param ... Arguments to pass to the underlying model function.
#' @keywords internal
#' @returns An object of S3 class `ordfor` as returned by
#'   [ordinalForest::ordfor()].

#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("ordinalForest")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # subsample to reduce runtime
#' house_data <- house_data[sample(nrow(house_data), nrow(house_data) / 10), ]
#' # fit wrapper
#' # (using inadvisedly few score sets and trees to reduce runtime)
#' ( fit_orig <- ordinalForest::ordfor(
#'   depvar = "Sat",
#'   data = house_data,
#'   nsets = 25, ntreefinal = 100
#' ) )
#' ( fit_wrap <- ordinalForest_wrapper(
#'   x = subset(house_data, select = -Sat),
#'   y = house_data$Sat,
#'   nsets = 25, ntreefinal = 100
#' ) )
#' @export
ordinalForest_wrapper <- function(x, y, ...) {
  rlang::check_installed("ordinalForest")
  # append response variable as column to predictor matrix
  x$.outcome <- y
  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "ordfor", .ns = "ordinalForest",
    depvar = ".outcome", data = rlang::expr(x), ...
  )
  rlang::eval_tidy(cl)
}
