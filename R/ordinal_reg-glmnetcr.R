#' Wrappers for `glmnetcr`
#'
#' The fit wrapper passes arguments to [glmnetcr::glmnetcr()].
#' The prediction wrapper selects predictions at a specific penalty value from
#' the regularization path.
#' @param x The predictor matrix.
#' @param y The outcome factor.
#' @param ... Additional arguments to pass to [glmnetcr::glmnetcr()].
#' @keywords internal
#' @returns An object of S3 class `glmnetcr` as returned by
#'   [glmnetcr::glmnetcr()], or a vector of class predictions
#'   or a tibble of probability predictions.

#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("glmnetcr")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_matrix <- model.matrix(
#'   Sat ~ Type + Infl + Cont + 0,
#'   data = house_data,
#'   contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
#' )
#' # fit wrapper
#' pen_vec <- 10 ^ seq(-1, -3, -.5)
#' ( fit_orig <- glmnetcr::glmnetcr(
#'   house_matrix, y = house_data$Sat,
#'   lambda = pen_vec
#' ) )
#' ( fit_wrap <- glmnetcr_wrapper(
#'   house_matrix, y = house_data$Sat,
#'   lambda = pen_vec
#' ) )
#' fit_tidy <-
#'   ordinal_reg() |>
#'   set_engine("glmnetcr") |>
#'   set_args(path_values = pen_vec, penalty = 0.01) |>
#'   fit(formula = Sat ~ Type + Infl + Cont + 0, data = house_data)
#' fit_tidy$fit
#' # predict wrapper
#' predict(
#'   fit_orig,
#'   newx = head(house_matrix)
#' )$probs[, , which.min(abs(fit_orig$lambda - 0.01))]
#' predict_glmnetcr_wrapper(
#'   fit_tidy$fit,
#'   newx = head(house_matrix),
#'   type = "prob",
#'   lambda = 0.01
#' )
#' predict(
#'   fit_orig,
#'   newx = head(house_matrix)
#' )$class[, which.min(abs(fit_orig$lambda - 0.01))]
#' predict_glmnetcr_wrapper(
#'   fit_tidy$fit,
#'   newx = head(house_matrix),
#'   type = "class",
#'   lambda = 0.01
#' )
#' @export
glmnetcr_wrapper <- function(
    x, y, weights = NULL,
    ...
) {
  rlang::check_installed("glmnetcr")

  if (is.null(weights)) {
    cl <- rlang::call2(
      .fn = "glmnetcr", .ns = "glmnetcr",
      x = rlang::expr(x), y = rlang::expr(y),
      ...
    )
  } else {
    cl <- rlang::call2(
      .fn = "glmnetcr", .ns = "glmnetcr",
      x = rlang::expr(x), y = rlang::expr(y),
      weights = rlang::expr(weights),
      ...
    )
  }
  rlang::eval_tidy(cl)
}

#' @rdname glmnetcr_wrapper
#' @param object A `glmnetcr` object.
#' @param newx A predictor matrix.
#' @param type Either `"class"` or `"prob"`.
#' @param lambda A penalty value at which to predict. If `NULL`, the step
#'   minimizing `criteria` is used.
#' @param criteria Criterion by which to select `lambda` within the path
#'   sequence. Defaults to `"bic"` for consistency with
#'   [glmnetcr::predict.glmnetcr()]. (NB: This contrasts with
#'   [predict_ordinalNet_wrapper()].)
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
    # FIXME: `predict.glmnetcr()` returns paths of criterion values, class
    # predictions, and probability predictions that track the penalty path
    # (`$lambda`). For this wrapper, linearly (or maybe logistically?)
    # interpolate between consecutive probabilities and then rescale them to add
    # to 1.
    s_idx <- which.min(abs(object$lambda - lambda))
  }

  switch(
    type,
    "class" = pred$class[, s_idx],
    "prob" = pred$probs[, , s_idx]
  )
}

# S3 methods for parsnip's model_fit dispatch ----------------------------------

#' @rdname glmnetcr_wrapper
#' @importFrom parsnip eval_args
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

#' @rdname glmnetcr_wrapper
#' @export
predict_class._glmnetcr <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

#' @rdname glmnetcr_wrapper
#' @export
predict_classprob._glmnetcr <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

# multi_predict methods --------------------------------------------------------

#' @rdname glmnetcr_wrapper
#' @importFrom parsnip eval_args multi_predict
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
