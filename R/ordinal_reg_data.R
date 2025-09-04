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
  # TODO: Translate between parsnip (standardized) and original values of these
  # parameters and ensure that engine limitations are handled correctly.
  # FIXME: (Pre-emptive tag for an expected error.)
  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "ordinal_link",
    original = "family",
    func = list(pkg = "dials", fun = "ordinal_link"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "ordinal_reg",
    eng = "ordinalNet",
    parsnip = "odds_link",
    original = "link",
    func = list(pkg = "dials", fun = "odds_link"),
    has_submodel = FALSE
  )

  # TODO: Work around `ordinalNet`'s handling of weights.
  parsnip::set_fit(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "ordinalNet", fun = "ordinalNet"),
      defaults = list(
        ordinal_link = "logit",
        odds_link = "cumulative_logit"
      )
    )
  )

  parsnip::set_encoding(
    model = "ordinal_reg",
    eng = "ordinalNet",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
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
      pre = NULL,
      post = NULL,
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
      post = NULL,
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
