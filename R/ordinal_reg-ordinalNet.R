#' Fit and predict wrappers for `ordinalNet`
#'
#' The fit wrapper restructures case weights and reorganizes arguments into a
#' call to [ordinalNet::ordinalNet()]. The prediction wrapper interpolates
#' between fitted penalties to enable submodel prediction at specified
#' penalties.
#' @param x The predictor data.
#' @param y The outcome vector.
#' @param weights An optional numeric vector of case weights. When provided, the
#'   outcome is restructured into a multinomial matrix of weighted indicators.
#' @param family The odds link function; either a standardized dial value
#'   (`"cumulative_link"`, `"adjacent_categories"`, `"continuation_ratio"`,
#'   `"stopping_ratio"`) or an `ordinalNet` native value (`"cumulative"`,
#'   `"acat"`, `"cratio"`, `"sratio"`).
#' @param link The ordinal link function; either a standardized dial value (e.g.
#'   `"logistic"`, `"probit"`) or a `ordinalNet` native value (e.g. `"logit"`,
#'   `"probit"`).
#' @param parallel_reg Logical; whether predictors share effects across
#'   thresholds. When `FALSE`, `parallelTerms` is set to `FALSE` and
#'   `nonparallelTerms` to `TRUE`.
#' @param parallelTerms Logical; whether to use parallel terms.
#' @param nonparallelTerms Logical; whether to use non-parallel terms.
#' @param parallelPenaltyFactor Numeric; scale factor applied to the penalty on
#'   parallel terms. Errs when used without parallel terms.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @returns An object of S3 class `ordinalNet` as returned by
#'   [ordinalNet::ordinalNet()], or a vector or matrix of predictions as
#'   returned by [`ordinalNet::predict.ordinalNet`]`()`.

#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("ordinalNet")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' house_matrix <- model.matrix(
#'   Sat ~ Type + Infl + Cont + 0,
#'   data = house_data,
#'   contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
#' )
#' pen_vec <- 10 ^ seq(-2.5, -.5, 1)
#' # fit wrapper
#' ( fit_orig <- ordinalNet::ordinalNet(
#'   house_matrix, y = house_data$Sat,
#'   family = "sratio", link = "logit",
#'   lambdaVals = pen_vec
#' ) )
#' ( fit_wrap <- ordinalNet_wrapper(
#'   house_matrix, y = house_data$Sat,
#'   family = "sratio", link = "logit",
#'   lambdaVals = pen_vec
#' ) )
#' fit_tidy <-
#'   ordinal_reg(ordinal_link = "logistic", odds_link = "stopping_ratio") %>%
#'   set_engine("ordinalNet") %>%
#'   set_args(path_values = pen_vec, penalty = 1) %>%
#'   fit(formula = Sat ~ Type + Infl + Cont + 0, data = house_data)
#' fit_tidy$fit
#' # predict wrapper
#' predict(
#'   fit_orig,
#'   newx = head(house_matrix),
#'   whichLambda = 2,
#'   type = "response"
#' )
#' predict_ordinalNet_wrapper(
#'   fit_tidy$fit,
#'   newx = head(house_matrix),
#'   type = "prob",
#'   lambda = pen_vec[2]
#' )
#' predict_ordinalNet_wrapper(
#'   fit_tidy$fit,
#'   newx = head(house_matrix),
#'   type = "prob",
#'   lambda = .01
#' )
#' @export
ordinalNet_wrapper <- function(
    x, y, weights = NULL,
    family = "cumulative",
    link = "logit",
    parallel_reg = NULL,
    parallelTerms = TRUE, nonparallelTerms = FALSE, parallelPenaltyFactor = 1,
    ...,
    call = rlang::caller_env()
) {
  rlang::check_installed("ordinalNet")

  # match standardized argument values to their `ordinalNet` natives
  family <- match_ordinal_family(family, call = call)
  link <- match_ordinal_link_ordinalNet(link, call = call)
  if (isFALSE(parallel_reg)) {
    parallelTerms <- FALSE
    nonparallelTerms <- TRUE
  }

  # throw error if penalty factor would go unused
  if (! parallelTerms && parallelPenaltyFactor != 1) {
    cli::cli_abort(
      "{.arg parallelPenaltyFactor} cannot be used without parallel terms."
    )
  }

  # restructure based on weights (requires `y` to be a factor)
  if (! is.null(weights)) {
    y_levs <- levels(y)
    y <- lapply(y_levs, function(u) (y == u) * weights)
    y <- do.call(cbind, y)
    colnames(y) <- y_levs
  }

  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "ordinalNet", .ns = "ordinalNet",
    x = rlang::expr(x), y = rlang::expr(y),
    family = rlang::expr(family), link = rlang::expr(link),
    parallelTerms = parallelTerms,
    nonparallelTerms = nonparallelTerms,
    parallelPenaltyFactor = parallelPenaltyFactor,
    ...
  )
  rlang::eval_tidy(cl)
}

match_ordinal_link_ordinalNet <- function(link, call = rlang::caller_env()) {
  if (! is.character(link)) {
    return(link)
  }
  check_string(link, arg = "ordinal_link", call = call)
  # native values pass through unchanged (note `logit`, not `logistic`)
  if (link %in% c("logit", "probit", "cloglog", "cauchit")) {
    return(link)
  }
  link <- rlang::arg_match0(
    link,
    dials::values_ordinal_link,
    arg_nm = "ordinal_link",
    error_call = call
  )
  if (link == "logistic") {
    link <- "logit"
  }
  if (link == "loglog") {
    cli::cli_abort(
      c(
        "The `ordinalNet` engine does not support the log-log ordinal link.",
        "i" = "See `?ordinalNet::ordinalNet` for provided link functions."
      ),
      call = call
    )
  }
  link
}

#' @rdname ordinalNet_wrapper
#' @export
predict_ordinalNet_wrapper <- function(
    object, newx, type, lambda, criteria = c("aic", "bic")
) {
  # REVIEW: This is necessary in order to prevent requiring the user to pass
  # a `penalty` value and nevertheless ignoring it.
  if (is.null(lambda)) {
    pred <- predict(
      object,
      newx = newx,
      whichLambda = NULL,
      criteria = criteria,
      type = "response"
    )
    res <- switch(
      type,
      "prob" = pred,
      # REVIEW: This "rounds down" if two probabilities are equal.
      "class" = apply(pred, 1L, which.max)
    )
    return(res)
  }

  # observed penalty adjacent to passed penalty
  obs_pen <- object$lambdaVals
  pen_ind <- adjacent_penalties(object, lambda)
  adj_pen <- obs_pen[pen_ind]

  # probability predictions based on adjacent penalty
  pred <- predict(
    object,
    newx = newx,
    whichLambda = pen_ind[1L],
    criteria = criteria,
    type = "response"
  )
  if (length(pen_ind) == 2L) {
    pred_high <- predict(
      object,
      newx = newx,
      whichLambda = pen_ind[2L],
      criteria = criteria,
      type = "response"
    )
    pred <- approx_prediction(pred, pred_high, adj_pen, lambda)
  }

  switch(
    type,
    "prob" = pred,
    # REVIEW: This "rounds down" if two probabilities are equal.
    "class" = apply(pred, 1L, which.max)
  )
}

# return 1 or 2 (adjacent) penalty path indices:
# if 1, it is used; if 2, predictions are interpolated
adjacent_penalties <- function(object, penalty) {

  # NB: `$lambdaVals` must be unique and decreasing.
  len <- length(object$lambdaVals)
  if (penalty < object$lambdaVals[len]) {
    return(len)
  } else if (penalty > object$lambdaVals[1L]) {
    return(1L)
  } else if (penalty %in% object$lambdaVals) {
    return(match(penalty, object$lambdaVals))
  } else {
    return(which(object$lambdaVals < penalty)[1L] + c(-1L, 0L))
  }
}

approx_prediction <- function(low, high, adjacent, penalty) {
  res <- low * NA_real_
  num_cls <- ncol(low)
  both <- cbind(low, high)
  for (i in 1:num_cls) {
    tmp <- both[, c(i, i + num_cls)]
    res[, i] <- apply(tmp, 1, approx_prediction_row, adjacent, penalty)
  }
  res <- apply(res, 1, function(x) x / sum(x))
  t(res)
}

approx_prediction_row <- function(values, adjacent, penalty) {
  approx(adjacent, values, xout = penalty)$y
}

# `ordinalNet` call stack using `predict()` when object has
# classes "_ordinalNet" and "model_fit":
#
# predict()
#  predict._ordinalNet(penalty = NULL)    <-- checks and sets penalty
#   predict.model_fit()                   <-- checks for extra vars in ...
#    predict_<type>()                     <-- dispatches by type
#     predict_<type>._ordinalNet()        <-- evaluates spec arguments
#      predict_<type>.model_fit()         <-- prepares tidy call
#       eval_tidy()                       <-- evaluates tidy call
#        predict_ordinalNet_wrapper()     <-- interpolates penalty
#         predict.ordinalNet()            <-- generates predictions

# `ordinalNet` call stack using `multi_predict()` when object has
# classes "_ordinalNet" and "model_fit":
#
# multi_predict()
#  multi_predict._ordinalNet()            <-- checks and sets penalty
#   multi_predict_<type>_ordinal_net()    <-- vectorizes over penalties
#    predict._ordinalNet(multi = FALSE)   <-- (see above)

# NB: `ordinalNet::predict.ordinalNet()` does not support multiple prediction,
# so `multi_predict()` merely vectorizes `predict(multi = FALSE)`.

#' @importFrom stats approx as.formula coef predict
#' @importFrom parsnip eval_args predict_raw multi_predict
#' @param penalty A numeric vector of penalty values.

#' @rdname ordinalNet_wrapper
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

  object$spec$args$penalty <-
    check_penalty_predict(penalty, object, multi)

  object$spec <- eval_args(object$spec)

  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

#' @rdname ordinalNet_wrapper
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
    check_penalty_predict(penalty, object, multi = TRUE)

  # adapted from `censored::multi_predict._coxnet`

  if (type != "raw" && length(opts) > 0L) {
    cli::cli_warn(
      "{.arg opts} is only used with {.arg type} = {.val raw} and was ignored."
    )
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

#' @rdname ordinalNet_wrapper
#' @export
predict_raw._ordinalNet <- function(object, new_data, opts = list(), ...)  {
  object$spec <- eval_args(object$spec)

  opts$whichLambda <- object$spec$args$penalty

  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

#' @rdname ordinalNet_wrapper
#' @export
predict_classprob._ordinalNet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

#' @rdname ordinalNet_wrapper
#' @export
predict_class._ordinalNet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

# adapted from `parsnip:::.check_glmnet_penalty_predict()`
check_penalty_predict <- function(
    penalty = NULL,
    object,
    multi = FALSE,
    call = rlang::caller_env()
) {
  engine <- object$spec$engine
  penalty_path_arg <- switch(
    engine,
    "ordinalNet" = "lambdaVals",
    "glmnetcr" = "lambda"
  )

  if (is.null(penalty)) {
    penalty <- object$fit[[penalty_path_arg]]
  }

  # when using `predict()`, allow for a single lambda
  if (! multi) {
    if (length(penalty) != 1) {
      cli::cli_abort(
        c(
          "{.arg penalty} should be a single numeric value.",
          "i" = "{.fn multi_predict} can be used to get multiple predictions
          per row of data."
        ),
        call = call
      )
    }
  }

  if (length(object$fit[[penalty_path_arg]]) == 1L &&
      penalty != object$fit[[penalty_path_arg]]) {
    cli::cli_abort(
      c(
        "The {.val {engine}} model was fit with a single penalty value of
        {.arg object$fit[[penalty_path_arg]]}. Predicting with a value of
        {.arg penalty} will give incorrect results from
        {.fn {paste0(engine, '()')}}."
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
      dplyr::mutate(penalty = .x) %>%
      dplyr::relocate(penalty)
  ) %>%
    tidyr::nest(.by = .row, .key = ".pred") %>%
    dplyr::select(-.row)
}

multi_predict_class_ordinal_net <- function(object, new_data, penalty) {
  purrr::map_dfr(
    penalty,
    ~ predict(object, new_data, type = "class", penalty = .x) %>%
      parsnip::add_rowindex() %>%
      dplyr::mutate(penalty = .x) %>%
      dplyr::relocate(penalty)
  ) %>%
    tidyr::nest(.by = .row, .key = ".pred") %>%
    dplyr::select(-.row)
}
