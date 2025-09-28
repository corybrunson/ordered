#' Wrappers for `ordinalNet`
#'
#' This wrapper converts the `family` options of [ordinalNet::ordinalNet()] to
#' the standardized `odds_link` options encoded in [dials::values_odds_link].
#' @param x The predictor data.
#' @param y The outcome vector.
#' @param ... Additional arguments to pass.
#' @keywords internal
#' @export
ordinal_net_wrapper <- function(
    x, y, weights = NULL,
    # REVIEW: Is this the preferred way to handle differences in parameter
    # names? See the commented alternative in `parsnip::translate.ordinal_reg`.
    # This solution `translate()`s to `ordered::ordinal_net_wrapper(...)`, which
    # is certainly disfavored against `ordinalNet::ordinalNet(...)`.
    # TODO: Test whether defaults can be omitted.
    family = "cumulative_logits", link = "logistic",
    ...
) {
  rlang::check_installed("ordinalNet")

  # match and convert odds link options
  family <- match.arg(
    family,
    c(
      "cumulative_logits",
      "adjacent_categories",
      "continuation_ratio",
      "stopping_ratio"
    )
  )
  family <- switch(
    family,
    cumulative_logits = "cumulative",
    adjacent_categories = "acat",
    continuation_ratio = "cratio",
    stopping_ratio = "sratio"
  )
  # REVIEW: There may be a standard way to do this. In particular, can this be
  # robust to upgrades in {ordinalNet}? How can errors and duplication be
  # prevented in tuning routines?
  link <- match.arg(
    link,
    c("logistic", "probit", "loglog", "cloglog", "cauchit")
  )
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

#' @rdname ordinal_net_wrapper
#' @export
predict_ordinal_net_wrapper <- function(object, newx, type, whichLambda) {
  # observed penalty adjacent to passed penalty
  obs_pen <- object$lambdaVals
  pen_ind <- adjacent_penalties(object, whichLambda)
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
    pred <- approx_prediction(pred, pred_high, adj_pen, whichLambda)
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

# These functions define the ordinal regression models.
# They are executed when this package is loaded via `.onLoad()`
# and modify the parsnip package's model environment.

# These functions are tested indirectly when the models are used.
# Since they are added to the parsnip model database on startup execution,
# they can't be test-executed so are excluded from coverage stats.

# nocov start

# ------------------------------------------------------------------------------
# `MASS::polr` components

make_ordinal_reg_polr <- function() {

  parsnip::set_model_engine("ordinal_reg", "classification", "polr")
  parsnip::set_dependency(
    "ordinal_reg",
    eng = "polr",
    pkg = "MASS",
    mode = "classification"
  )
  parsnip::set_dependency(
    "ordinal_reg",
    eng = "polr",
    pkg = "ordered",
    mode = "classification"
  )

  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "polr",
    parsnip = "ordinal_link",
    original = "method",
    func = list(pkg = "dials", fun = "ordinal_link"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "ordinal_reg",
    eng = "polr",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "MASS", fun = "polr"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "ordinal_reg",
    eng = "polr",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "polr",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "polr",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        tibble::as_tibble(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "probs"
        )
    )
  )

}

# ------------------------------------------------------------------------------
# `ordinalNet::ordinalNet` components

make_ordinal_reg_ordinalNet <- function() {

  parsnip::set_model_engine("ordinal_reg", "classification", "ordinalNet")
  parsnip::set_dependency(
    "ordinal_reg",
    eng = "ordinalNet",
    pkg = "ordered",
    mode = "classification"
  )
  parsnip::set_dependency(
    "ordinal_reg",
    eng = "ordinalNet",
    pkg = "ordinalNet",
    mode = "classification"
  )

  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "penalty",
    original = "lambdaVals",
    func = list(pkg = "dials", fun = "penalty"),
    # NOTE: Setting `has_submodel = TRUE` has the effect of calling
    # `multi_predict()` during tuning, even if a method doesn't exist.
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "ordinal_link",
    original = "link",
    func = list(pkg = "dials", fun = "ordinal_link"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "odds_link",
    original = "family",
    func = list(pkg = "dials", fun = "odds_link"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      # func = c(pkg = "ordinalNet", fun = "ordinalNet"),
      func = c(pkg = "ordered", fun = "ordinal_net_wrapper"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    options = list(
      # REVIEW: Should one-hot encoding be left to a recipe? `ordinalNet()` only
      # accepts numeric matrices, not data frames, so there is no "automatic"
      # pre- processing done there for {parsnip} to emulate. However, requiring
      # a recipe compromises the convenience of Tidymodels for comparing
      # multiple model engines. The practical problem is that
      # `predictor_indicators` doesn't seem to apply to `new_data` as handled
      # within tuning workflows.
      # https://www.tidymodels.org/learn/develop/models/
      # "What do I do about how my model handles factors or categorical data?"
      # https://www.tidyverse.org/blog/2020/07/parsnip-0-1-2/
      predictor_indicators = "one_hot",
      # REVIEW
      compute_intercept = TRUE,
      # REVIEW
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    type = "class",
    value = list(
      # https://www.tidymodels.org/learn/develop/models/
      # "Why would I preprocess my data?"
      # "Why would I post-process my predictions?"
      pre = NULL,
      post = function(x, object) {
        ordered(object$lvl[x], object$lvl)
      },
      func = c(fun = "predict_ordinal_net_wrapper"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "class",
          whichLambda = quote(object$spec$args$penalty)
        )
    )
  )

  parsnip::set_pred(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- tibble::as_tibble(x)
        x <- set_names(x, paste0(".pred_", object$lvl))
        x
      },
      func = c(fun = "predict_ordinal_net_wrapper"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "prob",
          whichLambda = quote(object$spec$args$penalty)
        )
    )
  )

  # # REVIEW: Only {censored} also enables `type = "linear_pred"`. Why is it not
  # # made available for all GLMs? Would it be useful here?
  # parsnip::set_pred(
  #   model = "ordinal_reg",
  #   eng = "ordinalNet",
  #   mode = "classification",
  #   type = "linear_pred",
  #   value = list(
  #     pre = NULL,
  #     post = function(x, object) {
  #       x <- tibble::as_tibble(x)
  #       nl <- length(object$lvl)
  #       x <- set_names(x, paste(
  #         ".pred_link",
  #         object$lvl[seq(nl - 1L)], object$lvl[seq(2L, nl)],
  #         sep = "_"
  #       ))
  #       x
  #     },
  #     func = c(fun = "predict"),
  #     args =
  #       list(
  #         object = quote(object$fit),
  #         newx = quote(new_data),
  #         type = "link"
  #       )
  #   )
  # )

}

# nocov end
