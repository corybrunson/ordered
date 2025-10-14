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
      func = c(pkg = "ordered", fun = "ordinalNet_wrapper"),
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
      func = c(fun = "predict_ordinalNet_wrapper"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "class",
          lambda = quote(object$spec$args$penalty)
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
      func = c(fun = "predict_ordinalNet_wrapper"),
      args =
        list(
          object = quote(object$fit),
          newx = quote(new_data),
          type = "prob",
          lambda = quote(object$spec$args$penalty)
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
