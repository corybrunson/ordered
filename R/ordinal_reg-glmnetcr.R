#' Prediction wrapper for `glmnetcr`
#'
#' Selects predictions at a specific penalty value from the regularization path.
#' When the requested penalty lies between two path values, linearly interpolates
#' between the probability matrices at the neighboring steps.
#' @inheritParams glmnetcr::glmnetcr
#' @param object A `glmnetcr` object.
#' @param newx A predictor matrix.
#' @param type Either `"class"` or `"prob"`.
#' @param lambda A penalty value at which to predict. If `NULL`, the step
#'   minimizing `criteria` is used.
#' @param criteria Criterion by which to select `lambda` within the path
#'   sequence. Defaults to `"bic"` for consistency with
#'   [glmnetcr::predict.glmnetcr()]. (NB: This contrasts with
#'   [predict_ordinalNet_wrapper()].)
#' @keywords internal
#' @returns A character vector of class predictions or a matrix of class
#'   probabilities.
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("glmnetcr")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_matrix <- model.matrix(
#'   Sat ~ Type + Infl + Cont + 0,
#'   data = house_data,
#'   contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
#' )
#' pen_vec <- 10 ^ seq(-1, -3, -.5)
#' fit <- glmnetcr::glmnetcr(
#'   house_matrix, y = house_data$Sat,
#'   lambda = pen_vec
#' )
#' # predictions (may disagree if `lambda` is not on the path `fit$lambda`)
#' # probability
#' predict(
#'   fit,
#'   newx = head(house_matrix)
#' )$probs[, , which.min(abs(fit$lambda - 0.02))]
#' predict_glmnetcr_wrapper(
#'   fit,
#'   newx = head(house_matrix),
#'   type = "prob",
#'   lambda = 0.02
#' )
#' # class
#' predict(
#'   fit,
#'   newx = head(house_matrix)
#' )$class[, which.min(abs(fit$lambda - 0.02))]
#' predict_glmnetcr_wrapper(
#'   fit,
#'   newx = head(house_matrix),
#'   type = "class",
#'   lambda = 0.02
#' )
#' @export
predict_glmnetcr_wrapper <- function(
    object, newx, type, lambda, criteria = c("bic", "aic")
) {
  # get all predictions
  pred <- predict(object, newx = newx)

  # select step index
  if (is.null(lambda)) {
    s_idx <- switch(
      criteria,
      "aic" = which.min(pred$AIC),
      "bic" = which.min(pred$BIC)
    )
  } else {
    # `predict.glmnetcr()` returns paths of criterion values, class predictions,
    # and probability predictions that track the penalty path (`$lambda`). This
    # wrapper linearly interpolates between consecutive probabilities and then
    # rescales them to add to 1.

    s_idx <- if (lambda < min(object$lambda)) {
      which.min(object$lambda)
    } else if (lambda > max(object$lambda)) {
      which.max(object$lambda)
    } else if (lambda %in% object$lambda) {
      match(lambda, object$lambda)
    } else {
      # NB: `$lambda` must be decreasing
      s0 <- max(which(object$lambda > lambda))
      s1 <- min(which(object$lambda < lambda))
      c(s0, s1)
    }
  }

  res <- if (length(s_idx) == 1L) {
    switch(
      type,
      "class" = pred$class[, s_idx],
      "prob" = pred$probs[, , s_idx]
    )
  } else {
    s0 <- s_idx[1]
    s1 <- s_idx[2]
    w <- (lambda - object$lambda[s0]) / (object$lambda[s1] - object$lambda[s0])
    probs <- (1 - w) * pred$probs[, , s0] + w * pred$probs[, , s1]
    switch(
      type,
      "class" = colnames(probs)[max.col(probs, ties.method = "first")],
      "prob" = sweep(probs, 1L, rowSums(probs), "/")
    )
  }

  return(res)
}

# S3 methods for parsnip's model_fit dispatch ----------------------------------

#' @rdname predict_glmnetcr_wrapper
#' @importFrom parsnip eval_args multi_predict
#' @param penalty A numeric penalty value. Overrides the penalty stored in the
#'   model specification.
#' @export
predict._glmnetcr <- function(
    object, new_data, type = NULL, opts = list(),
    penalty = NULL, ...
) {
  if (is.null(penalty) && !is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }
  object$spec$args$penalty <- penalty
  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

#' @rdname predict_glmnetcr_wrapper
#' @export
predict_class._glmnetcr <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

#' @rdname predict_glmnetcr_wrapper
#' @export
predict_classprob._glmnetcr <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

# multi_predict methods --------------------------------------------------------

#' @rdname predict_glmnetcr_wrapper
#' @param penalty A numeric vector of penalty values. Overrides the default
#'   penalty. If `NULL`, the regularization path stored in the model fit is
#'   used.
#' @export
multi_predict._glmnetcr <- function(
    object, new_data, type = NULL, opts = list(),
    penalty = NULL, ...
) {
  type <- check_pred_type(object, type)
  check_spec_pred_type(object, type)
  if (type == "prob") {
    check_spec_levels(object)
  }

  object$spec <- eval_args(object$spec)

  if (is.null(penalty)) {
    if (!is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambda
    }
  }

  if (type != "raw" && length(opts) > 0L) {
    rlang::warn("`opts` is only used with `type = 'raw'` and was ignored.")
  }

  pred <- switch(
    type,
    "prob" = multi_predict_classprob_glmnetcr(
      object, new_data = new_data, penalty = penalty
    ),
    "class" = multi_predict_class_glmnetcr(
      object, new_data = new_data, penalty = penalty
    ),
    "raw" = rlang::abort(
      "`type = 'raw'` is not yet supported for `multi_predict` with the `glmnetcr` engine."
    )
  )

  pred
}

multi_predict_classprob_glmnetcr <- function(object, new_data, penalty) {
  purrr::map_dfr(
    penalty,
    ~ predict(object, new_data, type = "prob", penalty = .x) |>
      tibble::as_tibble() |>
      parsnip::add_rowindex() |>
      dplyr::mutate(penalty = .x) |>
      dplyr::relocate(penalty)
  ) |>
    tidyr::nest(.by = .row, .key = ".pred") |>
    dplyr::select(-.row)
}

multi_predict_class_glmnetcr <- function(object, new_data, penalty) {
  purrr::map_dfr(
    penalty,
    ~ predict(object, new_data, type = "class", penalty = .x) |>
      parsnip::add_rowindex() |>
      dplyr::mutate(penalty = .x) |>
      dplyr::relocate(penalty)
  ) |>
    tidyr::nest(.by = .row, .key = ".pred") |>
    dplyr::select(-.row)
}
