#' Wrappers for `ordinalNet`
#'
#' The fit wrapper converts the standardized `odds_link` options encoded in
#' [`dials::values_odds_link`] to the `family` options of
#' [ordinalNet::ordinalNet()]. The prediction wrapper interpolates between
#' fitted penalties to enable submodel prediction at specified penalties.
#' @param x The predictor data.
#' @param y The outcome vector.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @export
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
#'   family = "stopping_ratio", link = "logistic",
#'   lambdaVals = pen_vec
#' ) )
#' fit_tidy <-
#'   ordinal_reg(ordinal_link = "logistic", odds_link = "stopping_ratio") |>
#'   set_engine("ordinalNet") |>
#'   set_args(path_values = pen_vec, penalty = 1) |>
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
#'
ordinalNet_wrapper <- function(
    x, y, weights = NULL,
    # REVIEW: Is this the preferred way to handle differences in parameter
    # names? See the commented alternative in `parsnip::translate.ordinal_reg`.
    # This solution `translate()`s to `ordered::ordinalNet_wrapper(...)`, which
    # is certainly disfavored against `ordinalNet::ordinalNet(...)`.
    # TODO: Test whether defaults can be omitted.
    family = "cumulative_link", link = "logistic",
    ...
) {
  rlang::check_installed("ordinalNet")

  # match and convert odds link options
  family <- match.arg(family, dials::values_odds_link)
  family <- switch(
    family,
    cumulative_link = "cumulative",
    adjacent_categories = "acat",
    continuation_ratio = "cratio",
    stopping_ratio = "sratio"
  )
  # REVIEW: There may be a standard way to do this. In particular, can this be
  # robust to upgrades in {ordinalNet}? How can errors and duplication be
  # prevented in tuning routines?
  link <- match.arg(link, dials::values_ordinal_link)
  if (link == "logistic") link <- "logit"
  if (link == "loglog") {
    cli::cli_abort(
      c(
        "The `ordinalNet` engine does not support the log-log ordinal link.",
        "i" = "See `?ordinalNet::ordinalNet` for provided link functions."
      )
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
    x = expr(x), y = expr(y),
    family = expr(family), link = expr(link),
    ...
  )
  rlang::eval_tidy(cl)
}

#' @rdname ordinalNet_wrapper
#' @export
predict_ordinalNet_wrapper <- function(object, newx, type, lambda) {
  # observed penalty adjacent to passed penalty
  obs_pen <- object$lambdaVals
  pen_ind <- adjacent_penalties(object, lambda)
  adj_pen <- obs_pen[pen_ind]

  # probability predictions based on adjacent penalty
  pred <- predict(
    object,
    newx = newx,
    whichLambda = pen_ind[1L],
    type = "response"
  )
  if (length(pen_ind) == 2L) {
    pred_high <- predict(
      object,
      newx = newx,
      whichLambda = pen_ind[2L],
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

# `use_extreme` is a placeholder for a policy that we need to set; do we error
# when predicting outside of the observed penalty range or do something else
# (such as predicting at the closest value in the path) --topepo

# See `translate.ordinal_reg()` in {parsnip}. by using `nLambda` and
# `lambdaMinRatio` together with `includeLambda0`, we ensure that any penalty
# value can be "interpolated" (those above the maximum are equivalent to the
# maximum). The `have_extr` variable determines whether this was done based on
# the arguments retained in the `ordinalNet` object. --corybrunson

adjacent_penalties <- function(object, penalty, use_extreme = TRUE) {
  ref <- object$lambdaVals
  in_rng <- penalty >= min(ref) && penalty <= max(ref)
  have_extr <- is.null(object$args$lambdaVals) && object$args$includeLambda0
  if (! in_rng && ! have_extr) {
    cli::cli_abort("The penalty value {format(penalty, digits = 3)} is
                    outside the penalty range contained in the model object.",
                   call = rlang::call2("predict"))
  }

  above <- which.min(ifelse(ref < penalty,  Inf, ref))
  below <- which.max(ifelse(ref > penalty, -Inf, ref))
  unique(sort(c(below, above)))
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
#        predict_ordinalNet_wrapper()     <-- interpolates penalty
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

  # TODO: Write unit test using `predict(multi = TRUE)`.
  object$spec$args$penalty <-
    .check_ordinalNet_penalty_predict(penalty, object, multi)

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
