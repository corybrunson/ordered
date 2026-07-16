#' Wrapper for `rms` predictions
#'
#' `rms::lrm()` and `rms::orm()` use [rms::predictrms()] internally, which
#' does not accept tibble inputs. This wrapper ensures that `newdata` is
#' converted to a plain data frame before the S3 method is dispatched.
#' @param object A fitted model object of class `"lrm"` or `"orm"`.
#' @param newdata A data frame or tibble of predictors.
#' @param type The prediction type, passed to the underlying method.
#' @param ... Additional arguments passed to the underlying method.
#' @keywords internal
#' @returns The result of [stats::predict()] dispatched on `object`.
#' @examplesIf rlang::is_installed("rms") && rlang::is_installed("MASS")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' ( fit <- rms::lrm(Sat ~ Infl + Type + Cont, data = house_data) )
#' # predict wrapper
#' predict(
#'   fit,
#'   newdata = head(house_data, 3),
#'   type = "fitted.ind"
#' )
#' # tibble input is converted internally
#' predict_lrm_wrapper(
#'   fit,
#'   newdata = tibble::as_tibble(head(house_data, 3)),
#'   type = "fitted.ind"
#' )
#' @export
predict_lrm_wrapper <- function(object, newdata, type, ...) {
  if (inherits(newdata, "tbl_df")) {
    newdata <- as.data.frame(newdata)
  }
  stats::predict(object, newdata = newdata, type = type, ...)
}
