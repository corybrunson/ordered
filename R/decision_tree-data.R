# These functions define the decision tree models.
# They are executed when this package is loaded via `.onLoad()`
# and modify the parsnip package's model environment.

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
  parsnip::set_dependency(
    "decision_tree",
    eng = "rpartScore",
    pkg = "ordered",
    mode = "classification"
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "rpartScore",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "rpartScore",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "rpartScore",
    parsnip = "cost_complexity",
    original = "cp",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      # func = c(pkg = "rpartScore", fun = "rpartScore"),
      func = c(pkg = "ordered", fun = "rpartScore_wrapper"),
      defaults = list()
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

  # NB: `predict()` accepts only `type = "vector"`.
  parsnip::set_pred(
    model = "decision_tree",
    eng = "rpartScore",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) {
        tibble::as_tibble(ordered(object$lvl[x], object$lvl))
      },
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "vector"
      )
    )
  )
}

# nocov end
