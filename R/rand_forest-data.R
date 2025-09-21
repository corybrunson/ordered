#' A wrapper for `ordinalForest`
#'
#' A wrapper is needed since they have a non-standard model interface that
#' required the data set and the column name (character string) for the
#' outcome.
#' @param x The predictor data.
#' @param y The outcome factor.
#' @param ... Arguments to pass to the underlying model function.
#' @export
#' @keywords internal
ordinal_forest_wrapper <- function(x, y, ...) {
  rlang::check_installed("ordinalForest")
  # append response variable as column to predictor matrix
  x$.outcome <- y
  # execute call on modified inputs
  cl <- rlang::call2(
    .fn = "ordfor", .ns = "ordinalForest",
    depvar = ".outcome", data = expr(x), ...
  )
  rlang::eval_tidy(cl)
}

# These functions define the random forest models.
# They are executed when this package is loaded via `.onLoad()`
# and modify the parsnip package's model environment.

# These functions are tested indirectly when the models are used.
# Since they are added to the parsnip model database on startup execution,
# they can't be test-executed so are excluded from coverage stats.

# nocov start

# ------------------------------------------------------------------------------
# `ordinalForest::ordfor` components

make_rand_forest_ordinalForest <- function() {

  parsnip::set_model_engine("rand_forest", "classification", "ordinalForest")
  parsnip::set_dependency(
    "rand_forest",
    eng = "ordinalForest",
    pkg = "ordinalForest",
    mode = "classification"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "ordinalForest",
    pkg = "ordered",
    mode = "classification"
  )

  # dials provided in {dials}
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "trees",
    original = "ntreefinal",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  # dials provided in {ordered}
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "naive_scores",
    original = "naive",
    func = list(pkg = "ordered", fun = "naive_scores"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "num_scores",
    original = "nsets",
    func = list(pkg = "ordered", fun = "num_scores"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "num_score_perms",
    original = "npermtrial",
    func = list(pkg = "ordered", fun = "num_score_perms"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "num_score_trees",
    original = "ntreeperdiv",
    func = list(pkg = "ordered", fun = "num_score_trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "num_scores_best",
    original = "nbest",
    func = list(pkg = "ordered", fun = "num_scores_best"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "ordinalForest",
    parsnip = "ord_metric",
    original = "perffunction",
    func = list(pkg = "ordered", fun = "ord_metric"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "ordinalForest",
    mode = "classification",
    value = list(
      interface = "data.frame",
      data = c(x = "x", y = "y"),
      protect = c("x", "y"),
      func = c(pkg = "ordered", fun = "ordinal_forest_wrapper"),
      defaults =
        list(
          num.threads = 1
          # perffunction = "probability"
        )
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "ordinalForest",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "ordinalForest",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = function(x, object) x$ypred,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "ordinalForest",
    mode = "classification",
    type = "prob",
    value = list(
      pre = function(x, object) {
        if (object$fit$perffunction != "probability") {
          cli::cli_abort(
            c(
              "The model fit does not appear to support class probabilities.",
              "i" = "Specify {.code ord_metric = 'probability'} for
                     probability predictions."
            ),
            call = rlang::call2("predict")
          )
        }
        x
      },
      post = function(x, object) {
        x <- x$classprobs
        colnames(x) <- object$lvl
        tibble::as_tibble(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
}
