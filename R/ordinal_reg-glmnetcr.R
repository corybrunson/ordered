#' Predict wrappers for `glmnetcr`
#'
#' Select predictions at a specific penalty value or a vector of penalty values.
#' For penalties between path values, linearly interpolate between the
#' probability matrices at the neighboring steps. For penalties outside the path
#' range, use the path endpoints.
#' @param object A `glmnetcr` object.
#' @param newx A predictor matrix.
#' @param type Either `"class"` or `"prob"`.
#' @param lambda A penalty value or vector at which to predict. If `NULL`, the
#'   step minimizing `criteria` is used (single penalty only).
#' @param criteria The criterion by which to select `lambda` within the path
#'   sequence. Defaults to `"bic"` for consistency with
#'   [glmnetcr::predict.glmnetcr()]. (NB: This contrasts with
#'   [predict_ordinalNet_wrapper()].)
#' @keywords internal
#' @returns `predict_glmnetcr_wrapper()` returns a character vector of class
#'   predictions or a matrix of class probabilities.
#'   `multi_predict_glmnetcr_wrapper()` returns a numeric array with one slice
#'   per penalty value for class probabilities, or a character matrix with one
#'   column per penalty value for class predictions.
#'   Both outputs mimic those of [glmnetcr::predict.glmnetcr()].

#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("glmnetcr")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_matrix <- model.matrix(
#'   Sat ~ Type + Infl + Cont + 0,
#'   data = house_data,
#'   contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
#' )
#' pen_vec <- 10 ^ seq(-1, -2, -.25)
#' fit <- glmnetcr::glmnetcr(
#'   house_matrix, y = house_data$Sat,
#'   lambda = pen_vec
#' )
#' # predictions (may disagree if `lambda` is not on the path `fit$lambda`)
#' set.seed(34091)
#' house_submatrix <- house_matrix[sample(nrow(house_matrix), 6), ]
#' # single, probability
#' predict(
#'   fit,
#'   newx = house_submatrix
#' )$probs[, , which.min(abs(fit$lambda - .02))]
#' predict_glmnetcr_wrapper(
#'   fit,
#'   newx = house_submatrix,
#'   type = "prob",
#'   lambda = .02
#' )
#' # single, class
#' predict(
#'   fit,
#'   newx = house_submatrix
#' )$class[, which.min(abs(fit$lambda - .02))]
#' predict_glmnetcr_wrapper(
#'   fit,
#'   newx = house_submatrix,
#'   type = "class",
#'   lambda = .02
#' )
#' # multiple, probabilities
#' predict(
#'   fit,
#'   newx = house_submatrix
#' )$probs
#' multi_predict_glmnetcr_wrapper(
#'   fit,
#'   newx = house_submatrix,
#'   type = "prob",
#'   lambda = c(.2, .02, .002)
#' )
#' # multiple, class
#' predict(
#'   fit,
#'   newx = house_submatrix
#' )$class
#' multi_predict_glmnetcr_wrapper(
#'   fit,
#'   newx = house_submatrix,
#'   type = "class",
#'   lambda = c(.2, .02, .002)
#' )
#' @export
predict_glmnetcr_wrapper <- function(
    object, newx, type, lambda, criteria = c("bic", "aic")
) {
  criteria <- match.arg(criteria)

  if (is.null(lambda)) {
    # select the step minimizing the criterion and predict at that penalty;
    # `AIC`/`BIC` are only available from `predict.glmnetcr()`
    pred <- predict(object, newx = newx)
    lambda <- object$lambda[switch(
      criteria,
      "aic" = which.min(pred$AIC),
      "bic" = which.min(pred$BIC)
    )]
  }

  # interpolate the penalty
  res <- multi_predict_glmnetcr_wrapper(object, newx, type, lambda)
  # return one slice/column
  if (type == "class") {
    res[, 1L]
  } else {
    res[, , 1L]
  }
}

#' @rdname predict_glmnetcr_wrapper
#' @export
multi_predict_glmnetcr_wrapper <- function(
    object, newx, type, lambda
) {
  type <- match.arg(type, c("prob", "class"))

  # a single call to `predict.glmnetcr()` returns arrays for the whole path
  pred <- predict(object, newx = newx)
  lams <- object$lambda

  # path step index, or pair of bracketing step indices, for a penalty value
  s_idx <- lapply(lambda, function(lam) {
    if (lam < min(lams)) {
      which.min(lams)
    } else if (lam > max(lams)) {
      which.max(lams)
    } else if (lam %in% lams) {
      match(lam, lams)
    } else {
      # NB: `$lambda` must be decreasing
      c(max(which(lams > lam)), min(which(lams < lam)))
    }
  })

  # probability matrix at a penalty value, linearly interpolated between
  # bracketing steps and rescaled to add to 1
  probs_at <- function(lam, idx) {
    if (length(idx) == 1L) {
      pred$probs[, , idx]
    } else {
      w <- (lam - lams[idx[1L]]) / (lams[idx[2L]] - lams[idx[1L]])
      probs <- (1 - w) * pred$probs[, , idx[1L]] + w * pred$probs[, , idx[2L]]
      sweep(probs, 1L, rowSums(probs), "/")
    }
  }

  n <- nrow(newx)
  n_pen <- length(lambda)

  if (type == "prob") {
    res <- array(NA_real_, c(n, ncol(pred$probs), n_pen))
    for (k in seq_len(n_pen)) {
      res[, , k] <- probs_at(lambda[k], s_idx[[k]])
    }
    dimnames(res) <- list(NULL, colnames(pred$probs), NULL)
  } else {
    res <- matrix(NA_character_, nrow = n, ncol = n_pen)
    for (k in seq_len(n_pen)) {
      probs_k <- probs_at(lambda[k], s_idx[[k]])
      res[, k] <- colnames(probs_k)[max.col(probs_k, ties.method = "first")]
    }
    colnames(res) <- as.character(lambda)
  }
  res
}

# `glmnetcr` call stack using `predict()` when object has
# classes "_glmnetcr" and "model_fit":
#
# predict()
#  predict._glmnetcr(penalty = NULL)       <-- checks and sets penalty
#   predict.model_fit()                    <-- checks for extra vars in ...
#    predict_<type>()                      <-- dispatches by type
#     predict_<type>._glmnetcr()           <-- evaluates spec arguments
#      predict_<type>.model_fit()          <-- prepares tidy call
#       eval_tidy()                        <-- evaluates tidy call
#        predict_glmnetcr_wrapper()        <-- selects lambda and delegates
#         multi_predict_glmnetcr_wrapper() <-- interpolates a penalty vector
#          predict.glmnetcr()              <-- generates predictions

# `glmnetcr` call stack using `multi_predict()` when object has
# classes "_glmnetcr" and "model_fit":
#
# multi_predict()
#  multi_predict._glmnetcr()               <-- checks/encodes, sets penalty
#   multi_predict_<type>_glmnetcr()        <-- formats per row and penalty
#    multi_predict_glmnetcr_wrapper()      <-- interpolates a penalty vector
#     predict.glmnetcr()                   <-- generates predictions

# NB: `glmnetcr::predict.glmnetcr()` returns predictions for every step in the
# fitted penalty path; `multi_predict_glmnetcr_wrapper()` calls it once and
# matches or linearly interpolates between steps for each requested penalty.

# S3 methods for parsnip's model_fit dispatch ----------------------------------

#' @rdname predict_glmnetcr_wrapper
#' @importFrom parsnip eval_args multi_predict
#' @param penalty A numeric penalty value. Overrides the penalty stored in the
#'   model specification.
#' @export
predict._glmnetcr <- function(
    object,
    new_data,
    type = NULL,
    opts = list(),
    penalty = NULL,
    multi = FALSE,
    ...
) {
  if (is.null(penalty) && ! is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }

  object$spec$args$penalty <-
    check_penalty_predict(penalty, object, multi)

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
    object,
    new_data,
    type = NULL,
    opts = list(),
    penalty = NULL,
    ...
) {
  type <- check_pred_type(object, type)
  check_spec_pred_type(object, type)
  if (type == "prob") {
    check_spec_levels(object)
  }

  if (is.null(penalty)) {
    if (! is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambda
    }
  }

  object$spec$args$penalty <-
    check_penalty_predict(penalty, object, multi = TRUE)

  if (type != "raw" && length(opts) > 0L) {
    cli::cli_warn(
      "{.arg opts} is only used with {.arg type} = {.val raw} and was ignored."
    )
  }

  pred <- switch(
    type,
    "prob" = multi_predict_classprob_glmnetcr(
      object$fit,
      newx = parsnip::prepare_data(object, new_data),
      penalty = penalty
    ),
    "class" = multi_predict_class_glmnetcr(
      object$fit,
      newx = parsnip::prepare_data(object, new_data),
      penalty = penalty,
      lvl = object$lvl
    ),
    "raw" = cli::cli_abort(
      "{.arg type} = {.val raw} is not yet supported for `multi_predict`
      with the {.val glmnetcr} engine."
    )
  )

  pred
}

multi_predict_classprob_glmnetcr <- function(object, newx, penalty) {
  probs <- multi_predict_glmnetcr_wrapper(
    object, newx, type = "prob", lambda = penalty
  )
  lbls <- paste0(".pred_", dimnames(probs)[[2]])
  nested <- lapply(seq_len(nrow(newx)), function(i) {
    stats::setNames(tibble::as_tibble(t(probs[i, , ])), lbls) |>
      tibble::add_column(penalty = penalty, .before = 1L)
  })
  tibble::tibble(.pred = nested)
}

multi_predict_class_glmnetcr <- function(object, newx, penalty, lvl) {
  classes <- multi_predict_glmnetcr_wrapper(
    object, newx, type = "class", lambda = penalty
  )
  nested <- lapply(seq_len(nrow(newx)), function(i) {
    tibble::tibble(
      penalty = penalty,
      .pred_class = ordered(unname(classes[i, ]), levels = lvl)
    )
  })
  tibble::tibble(.pred = nested)
}
