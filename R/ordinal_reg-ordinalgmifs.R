#' Fit wrappers for `ordinalgmifs`
#'
#' These wrappers reorganize arguments into calls to
#' [ordinalgmifs::ordinalgmifs()] and [ordinalgmifs::predict.ordinalgmifs()].
#'
#' The fit wrapper matches standardized argument values used by
#' `parsnip::ordinal_reg()` to values native to `ordinalgmifs` within the
#' wrappers. It also stores options required for prediction that cannot be
#' accepted by [parsnip::predict.model_fit()].
#'
#' **NB:** Some combinations of `probability.model` and `link` are rejected,
#' while others warn and continue using the link supported by `ordinalgmifs`;
#' these wrappers do not validate model and link compatibility in advance.
#'
#' **NB:** The continuation ratio model family in `ordinalgmifs` corresponds to
#' the stopping ratio family in `VGAM` and in `ordinalNet`. Accordingly, while
#' the wrapper preserves behavior for the native valuses `"ForwardCR"` and
#' `"BackwardCR"`, it converts the [parsnip::ordinal_reg()] value
#' `"stopping_ratio"` to one of these (depending on `reverse`) and errs when
#' `"continuation_ratio"` is passed. `reverse` itself is non-native and errs in
#' combination with any other family.
#'
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param x Penalized predictors. `NULL`, a character vector of column names in
#'   `data`, or a matrix or data frame with column names.
#' @param probability.model The odds link function; either a standardized dial
#'   value (`"cumulative_link"`, `"adjacent_categories"`, `"stopping_ratio"`,
#'   `"stereotype"`) or an `ordinalgmifs` native value (`"Cumulative"`,
#'   `"AdjCategory"`, `"ForwardCR"`, `"BackwardCR"`, `"Stereotype"`).
#' @param link The ordinal link function; either a standardized dial value
#'   (e.g. `"logistic"`, `"cloglog"`) or an `ordinalgmifs` native value (e.g.
#'   `"logit"`, `"cloglog"`).
#' @param reverse Logical; whether to use the backward rather than forward
#'   stopping-ratio model. Only compatible with `odds_link = "stopping_ratio"`
#'   and with the `probability.model` values `"ForwardCR"` and `"BackwardCR"`.
#' @param ... Additional arguments to pass to [ordinalgmifs::ordinalgmifs()] or
#'   [ordinalgmifs::predict.ordinalgmifs()].
#' @param model.select The model selection rule used during prediction.
#' @param call The calling environment.
#' @returns `ordinalgmifs_wrapper()` returns an object of class `ordinalgmifs`.
#'   `predict_ordinalgmifs_wrapper()` returns a list with predicted class
#'   probabilities and classes.
#' @keywords internal
#' @export
ordinalgmifs_wrapper <- function(
  formula,
  data,
  x = NULL,
  probability.model = "Cumulative",
  link = "logit",
  reverse = FALSE,
  ...,
  model.select = "AIC",
  call = rlang::caller_env()
) {
  rlang::check_installed("ordinalgmifs")
  probability.model <-
    match_odds_link_ordinalgmifs(probability.model, reverse, call = call)
  link <- match_ordinal_link_ordinalgmifs(link, call = call)

  penalized <- ordinalgmifs_x_names(x, data)
  split <- ordinalgmifs_formula_split(formula, data, penalized)
  x_matrix <- ordinalgmifs_x_matrix(x, data)

  cl <- rlang::call2(
    .fn = "ordinalgmifs",
    .ns = "ordinalgmifs",
    formula = split$formula,
    data = data,
    x = x_matrix,
    probability.model = probability.model,
    link = link,
    ...
  )
  fit <- rlang::eval_tidy(cl)
  fit$.ordered_x_names <- penalized
  fit$.ordered_neww <- split$neww
  fit$.ordered_model_select <- model.select
  fit
}

#' @rdname ordinalgmifs_wrapper
#' @export
predict_ordinalgmifs_wrapper <- function(object, newdata, ...) {
  rlang::check_installed("ordinalgmifs")
  dots <- list(...)

  if ("model.select" %in% names(dots)) {
    model.select <- dots$model.select
    dots$model.select <- NULL
  } else {
    model.select <- object$.ordered_model_select
  }

  penalized <- object$.ordered_x_names
  x_matrix <- if (length(penalized) == 0L) {
    NULL
  } else {
    as.matrix(newdata[, penalized, drop = FALSE])
  }
  if (!is.null(x_matrix)) {
    rownames(x_matrix) <- NULL
  }
  unpenalized <- newdata[, setdiff(names(newdata), penalized), drop = FALSE]

  cl <- rlang::call2(
    .fn = "predict",
    .ns = "stats",
    object = object,
    neww = object$.ordered_neww,
    newdata = unpenalized,
    newx = x_matrix,
    model.select = model.select,
    !!!dots
  )
  rlang::eval_tidy(cl)
}

match_odds_link_ordinalgmifs <- function(
  x,
  reverse = FALSE,
  call = rlang::caller_env()
) {
  if (length(x) != 1L || !is.character(x) || is.na(x)) {
    cli::cli_abort(
      "{.arg probability.model} must be a single character value.",
      call = call
    )
  }
  if (length(reverse) != 1L || !is.logical(reverse) || is.na(reverse)) {
    cli::cli_abort("{.arg reverse} must be `TRUE` or `FALSE`.", call = call)
  }
  if (x == "continuation_ratio") {
    cli::cli_abort(
      c(
        paste0(
          "{.arg odds_link} cannot be {.val continuation_ratio} for the ",
          "{.val ordinalgmifs} engine."
        ),
        "i" = paste0(
          "Its native {.val ForwardCR} and {.val BackwardCR} models use ",
          "stopping ratios."
        ),
        "i" = paste0(
          "Use {.arg odds_link} = {.val stopping_ratio} with ",
          "{.arg reverse} = `FALSE` or `TRUE`."
        )
      ),
      call = call
    )
  }
  if (x == "stopping_ratio" && !reverse) {
    return("ForwardCR")
  }
  if (reverse) {
    if (x %in% c("stopping_ratio", "ForwardCR")) {
      return("BackwardCR")
    }
    if (x == "BackwardCR") {
      return("ForwardCR")
    }
    cli::cli_abort(
      c(
        paste0(
          "{.arg reverse} can only be used with {.arg odds_link} = ",
          "{.val stopping_ratio} or native {.arg probability.model} values ",
          "{.val ForwardCR} and {.val BackwardCR}."
        ),
        "i" = "Set {.arg reverse} = `FALSE` or omit it for other odds links."
      ),
      call = call
    )
  }
  values <- c(
    cumulative_link = "Cumulative",
    adjacent_categories = "AdjCategory",
    stereotype = "Stereotype"
  )
  if (x %in% names(values)) values[[x]] else x
}

match_ordinal_link_ordinalgmifs <- function(x, call = rlang::caller_env()) {
  if (length(x) != 1L || !is.character(x) || is.na(x)) {
    cli::cli_abort(
      "{.arg link} must be a single character value.",
      call = call
    )
  }
  values <- c(
    logistic = "logit",
    probit = "probit",
    cloglog = "cloglog",
    loge = "loge"
  )
  if (x %in% names(values)) values[[x]] else x
}

# resolve predictor names from `x` input
ordinalgmifs_x_names <- function(x, data, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(character())
  }
  if (is.character(x)) {
    missing <- setdiff(x, names(data))
    if (length(missing) > 0L) {
      cli::cli_abort(
        "{.arg x} names {.val {missing}} not found in {.arg data}.",
        call = call
      )
    }
    return(x)
  }
  if (is.data.frame(x) || is.matrix(x)) {
    nms <- colnames(x)
    if (is.null(nms)) {
      cli::cli_abort("{.arg x} must have column names.", call = call)
    }
    return(nms)
  }
  cli::cli_abort(
    "{.arg x} must be `NULL`, a character vector, a data frame, or a matrix.",
    call = call
  )
}

# assemble row-unnamed penalized predictor matrix
ordinalgmifs_x_matrix <- function(x, data) {
  res <- if (is.null(x)) {
    NULL
  } else if (is.character(x)) {
    as.matrix(data[, x, drop = FALSE])
  } else {
    as.matrix(x)
  }
  if (!is.null(res)) {
    rownames(res) <- NULL
  }
  res
}

# derive separate formulae for fit and for prediction
ordinalgmifs_formula_split <- function(formula, data, penalized) {
  labels <- attr(stats::terms(formula, data = data), "term.labels")
  keep <- setdiff(labels, penalized)
  lhs <- paste(
    deparse(formula[[2L]], width.cutoff = 500L),
    collapse = " "
  )
  rhs <- if (length(keep) == 0L) "1" else paste(keep, collapse = " + ")
  env <- environment(formula)
  list(
    formula = stats::as.formula(paste(lhs, "~", rhs), env = env),
    neww = stats::as.formula(paste("~", rhs), env = env)
  )
}
