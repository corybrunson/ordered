#' A wrapper for `rpartScore`
#'
#' A wrapper is used because the model interface requires the response variable
#' to be numeric rather than ordered or factor; the wrapper edits the input
#' `data` accordingly.
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param ... Additional arguments to pass.
#' @export
#' @keywords internal
rpart_score_wrapper <- function(formula, data, ...) {
  rlang::check_installed("rpartScore")
  # convert response variable in `data` from ordinal to integer
  lhs <- rlang::f_lhs(formula)
  data[[lhs]] <- as.integer(data[[lhs]])
  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "rpartScore", .ns = "rpartScore",
    formula = expr(formula), data = expr(data), ...
  )
  rlang::eval_tidy(cl)
}

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
  # REVIEW: Adopting `*_func` as a naming convention for named function options.
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "rpartScore",
    parsnip = "split_func",
    original = "split",
    func = list(pkg = "ordered", fun = "split_func"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "rpartScore",
    parsnip = "prune_func",
    original = "prune",
    func = list(pkg = "ordered", fun = "prune_func"),
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
      func = c(pkg = "ordered", fun = "rpart_score_wrapper"),
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
