# mimicking file of same name in {censored}

pred_types <- c(
  "raw", "numeric", "class", "prob", "conf_int", "pred_int",
  "quantile", "time", "survival", "linear_pred", "hazard"
)

check_pred_type <- function (object, type, ..., call = rlang::caller_env()) {
  if (is.null(type)) {
    type <- switch(
      object$spec$mode,
      regression = "numeric",
      classification = "class",
      `censored regression` = "time",
      `quantile regression` = "quantile",
      cli::cli_abort("{.arg type} should be one of {.or {.val {all_modes}}}.",
                     call = call)
    )
  }
  if (!(type %in% pred_types))
    cli::cli_abort("{.arg type} should be one of {.or {.arg {pred_types}}}.",
                   call = call)
  switch(
    type,
    numeric = if (object$spec$mode != "regression") {
      cli::cli_abort("For numeric predictions,
                     the object should be a regression model.",
                     call = call)
    },
    class = if (object$spec$mode != "classification") {
      cli::cli_abort("For class predictions,
                     the object should be a classification model.",
                     call = call)
    },
    prob = if (object$spec$mode != "classification") {
      cli::cli_abort("For probability predictions,
                     the object should be a classification model.",
                     call = call)
    },
    time = if (object$spec$mode != "censored regression") {
      cli::cli_abort("For event time predictions,
                     the object should be a censored regression.",
                     call = call)
    },
    survival = if (object$spec$mode != "censored regression") {
      cli::cli_abort("For survival probability predictions,
                     the object should be a censored regression.",
                     call = call)
    },
    hazard = if (object$spec$mode != "censored regression") {
      cli::cli_abort("For hazard predictions,
                     the object should be a censored regression.",
                     call = call)
    },
    linear_pred = if (object$spec$mode != "censored regression") {
      cli::cli_abort("For the linear predictor,
                     the object should be a censored regression.",
                     call = call)
    })
  type
}

check_spec_levels <- function (spec) {
  if ("class" %in% spec$lvl) {
    cli::cli_abort(
      c(
        "The outcome variable {.var {spec$preproc$y_var}}
        has a level called {.val class}.",
        i = "This value is reserved for parsnip's classification internals;
        please change the levels, perhaps with {.fn forcats::fct_relevel}.",
        call = NULL
      )
    )
  }
}

check_spec_pred_type <- function (object, type, call = caller_env()) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    cli::cli_abort("No {.val {type}} prediction method available for this model.
                   {.arg type} should be one of: {.val {possible_preds}}.",
                   call = call)
  }
  invisible(NULL)
}

spec_has_pred_type <- function (object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}
