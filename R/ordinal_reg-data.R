#' A wrapper for `ordinalNet`
#'
#' This wrapper converts the `family` options of [ordinalNet::ordinalNet()] to
#' the standardized `odds_link` options encoded in [dials::values_odds_link].
#' @param x The predictor data.
#' @param y The outcome vector.
#' @param ... Additional arguments to pass.
#' @export
#' @keywords internal
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
        "The `ordinalNet` engine does not provide a log-log ordinal link.",
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

  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "penalty",
    original = "lambdaVals",
    func = list(pkg = "dials", fun = "penalty"),
    # NOTE: Setting `has_submodel = TRUE` has the effect of calling
    # `multi_predict()` during tuning, even if a method doesn't exist.
    # TODO: Once working, build out ordinalNet methods to include submodels.
    # has_submodel = TRUE
    has_submodel = FALSE
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
      # REVIEW: Should this be left to a recipe? `ordinalNet()` only accepts
      # numeric matrices, not data frames, so there is no "automatic" pre-
      # processing done there for {parsnip} to emulate. However, requiring a
      # recipe compromises the convenience of Tidymodels for comparing multiple
      # model engines. The practical problem is that `predictor_indicators`
      # doesn't seem to apply to `new_data` as handled within tuning workflows.
      # https://www.tidymodels.org/learn/develop/models/
      # "What do I do about how my model handles factors or categorical data?"
      # https://www.tidyverse.org/blog/2020/07/parsnip-0-1-2/
      # predictor_indicators = "none",
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
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "class"
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
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "response"
        )
    )
  )

}

# nocov end
