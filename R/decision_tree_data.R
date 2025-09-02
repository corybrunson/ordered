# These functions define the decision tree models.
# They are executed when this package is loaded via `.onLoad()`
# and modify the {parsnip} package's model environment.

# These functions are tested indirectly when the models are used.
# Since they are added to the parsnip model database on startup execution,
# they can't be test-executed so are excluded from coverage stats.

# nocov start

# ------------------------------------------------------------------------------
# `rpartScore::rpartScore` components

make_decision_tree_rpartScore <- function() {

  parsnip::set_model_engine("decision_tree", "classification", "rpartScore")
  parsnip::set_dependency(
    "decision_tree",
    eng = "rpartScore",
    pkg = "rpartScore",
    mode = "classification"
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rpartScore", fun = "rpartScore"),
      defaults = list(
        split = "abs",
        prune = "mc"
      )
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      # TODO: Test nullifying this if `type` is removed below.
      post = function(x, object) { as_tibble(x) },
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        # TODO: Test removing this per {parsnip} specification.
        type = "prob"
      )
    )
  )
}

# nocov end
