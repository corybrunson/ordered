# ordinalNet call stack using `predict()` when object has
# classes "_ordinalNet" and "model_fit":
#
# predict()
#  predict._ordinalNet(penalty = NULL)    <-- checks and sets penalty
#   predict.model_fit()                   <-- checks for extra vars in ...
#    predict_<type>()                     <-- dispatches by type
#     predict_<type>._ordinalNet()        <-- post-processes interpolation
#      predict_<type>.model_fit()         <-- prepares tidy call
#       eval_tidy()                       <-- evaluates tidy call
#        predict_ordinal_net_wrapper()    <-- interpolates penalty
#         predict.ordinalNet()            <-- generates predictions

# ordinalNet call stack using `multi_predict()` when object has
# classes "_ordinalNet" and "model_fit":
#
# multi_predict()
#  multi_predict._ordinalNet()            <-- checks and sets penalty
#   multi_predict_<type>_ordinal_net()    <-- vectorizes prediction over penalty
#    predict._ordinalNet(multi = FALSE)   <-- (see above)

#' @importFrom stats approx predict
#' @importFrom parsnip eval_args predict_raw multi_predict
#' @param penalty A numeric vector of penalty values.

#' @rdname ordinal_net_wrapper
#' @export
predict._ordinalNet <- function(
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

  # TODO: Write unit test using `predict(multi = TRUE)`.
  object$spec$args$penalty <-
    .check_ordinalNet_penalty_predict(penalty, object, multi)

  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

#' @rdname ordinal_net_wrapper
#' @export
multi_predict._ordinalNet <- function(
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

  object$spec <- eval_args(object$spec)

  if (is.null(penalty)) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (! is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambdaVals
    }
  }

  object$spec$args$penalty <-
    .check_ordinalNet_penalty_predict(penalty, object, multi = TRUE)

  # adapted from `censored::multi_predict._coxnet`

  if (type != "raw" && length(opts) > 0L) {
    rlang::warn("`opts` is only used with `type = 'raw'` and was ignored.")
  }

  pred <- switch(
    type,
    "prob" = multi_predict_classprob_ordinal_net(
      object, new_data = new_data, penalty = penalty
    ),
    "class" = multi_predict_class_ordinal_net(
      object, new_data = new_data, penalty = penalty
    ),
    "raw" = predict(
      object, new_data = new_data, type = "raw",
      opts = opts, penalty = penalty, multi = TRUE
    )
  )

  pred
}

#' @rdname ordinal_net_wrapper
#' @export
predict_raw._ordinalNet <- function(object, new_data, opts = list(), ...)  {
  object$spec <- eval_args(object$spec)

  opts$whichLambda <- object$spec$args$penalty

  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

#' @rdname ordinal_net_wrapper
#' @export
predict_classprob._ordinalNet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

#' @rdname ordinal_net_wrapper
#' @export
predict_class._ordinalNet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

.check_ordinalNet_penalty_predict <- function(
    penalty = NULL, object, multi = FALSE, call = rlang::caller_env()
) {
  if (is.null(penalty)) {
    penalty <- object$fit$lambdaVals
  }

  if (multi) {
    penalty <- sort(unique(penalty))
    # REVIEW: This code prevents `tune_grid()` from working.
    # if (length(penalty) < 2L) {
    #   cli::cli_abort("There should be at least 2 penalty values for
    #                {.fn multi_predict}; please use {.fn predict}) instead.",
    #                  call = call)
    # }
  } else {
    if (length(penalty) != 1L) {
      cli::cli_abort(
        c(
          "{.arg penalty} should be a single numeric value.",
          "i" = "{.fn multi_predict} can be used to get
          multiple predictions per row of data."
        ),
        call = call
      )
    }
  }

  if (length(object$fit$lambdaVals) == 1L && penalty != object$fit$lambdaVals) {
    cli::cli_abort(
      c(
        "The ordinalNet model was fit with a single penalty value of
      {.arg object$fit$lambdaVals}. Predicting with a value of {.arg penalty}
      will give incorrect results from `ordinalNet()`."
      ),
      call = call
    )
  }

  penalty
}

multi_predict_classprob_ordinal_net <- function(object, new_data, penalty) {
  purrr::map_dfr(
    penalty,
    ~ predict(object, new_data, type = "prob", penalty = .x) %>%
      tibble::as_tibble() %>%
      parsnip::add_rowindex() %>%
      dplyr::mutate(penalty = .x) %>% relocate(penalty)
  ) %>%
    tidyr::nest(.by = .row, .key = ".pred") %>%
    dplyr::select(-.row)
}

multi_predict_class_ordinal_net <- function(object, new_data, penalty) {
  purrr::map_dfr(
    penalty,
    ~ predict(object, new_data, type = "class", penalty = .x) %>%
      parsnip::add_rowindex() %>%
      dplyr::mutate(penalty = .x) %>% relocate(penalty)
  ) %>%
    tidyr::nest(.by = .row, .key = ".pred") %>%
    dplyr::select(-.row)
}
