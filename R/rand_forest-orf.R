#' Fit wrapper for `orf`
#'
#' The `orf` package uses a non-standard interface that requires the predictor
#' matrix `X` and the outcome vector `Y` as separate arguments, with `Y` as a
#' numeric vector. This wrapper converts the parsnip-standard data.frame (`X`)
#' and factor (`Y`) to the required formats before calling [orf::orf()].
#' @param X The predictor data (data.frame).
#' @param Y The outcome factor.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @returns An object of S3 class `orf` as returned by [orf::orf()].
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("orf")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_data <- house_data[sample(nrow(house_data), 50), ]
#' # encode factors as dummy variables before passing to wrapper
#' X_mat <- model.matrix(~ Infl + Type + Cont - 1, data = house_data)
#' # fit wrapper
#' ( fit_wrap <- orf_wrapper(
#'   X = X_mat,
#'   Y = house_data$Sat,
#'   num.trees = 10
#' ) )
#' @export
orf_wrapper <- function(X, Y, ...) {
  rlang::check_installed("orf")
  X <- as.matrix(X)
  Y <- as.numeric(Y)

  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "orf", .ns = "orf",
    X = X, Y = Y, ...
  )
  rlang::eval_tidy(cl)
}

# post-process `orf` class predictions: convert `orf.prediction` object
# to a factor vector
orf_class_post <- function(x, object) {
  pred_class <- x$predictions[, 1]
  factor(pred_class, levels = seq_along(object$lvl), labels = object$lvl)
}

# post-process `orf` probability predictions: convert `orf.prediction`
# object to a tibble of class probabilities
orf_prob_post <- function(x, object) {
  x <- x$predictions
  colnames(x) <- paste0(".pred_", object$lvl)
  tibble::as_tibble(x)
}
