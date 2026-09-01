#' Fit wrapper for `ordinal::clm()`
#'
#' Fit `clm()`, translating `parallel_reg` into the `nominal` formula. This is
#' done here rather than in [parsnip::translate()] because the formula can only
#' be split once `data` is known, and because the standard translation
#' (`make_form_call()`) overwrites any location formula that `translate()` sets.
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param weights An optional vector of case weights to pass.
#' @param parallel_reg Logical; whether predictor effects are shared across
#'   thresholds. When `FALSE`, every predictor is moved to a `nominal` formula.
#' @param ... Additional arguments to pass.
#' @param call The execution environment of a currently running function.
#' @keywords internal
#' @returns An object of class `clm` as returned by [ordinal::clm()].
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("ordinal")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # arguments are translated
#' ( fit_orig <- ordinal::clm(
#'   Sat ~ Type + Cont, data = house_data,
#'   link = "logit", threshold = "symmetric2"
#' ) )
#' ( fit_wrap <- clm_wrapper(
#'   Sat ~ Type + Cont, data = house_data,
#'   link = "logistic", threshold = "symmetric_zero"
#' ) )
#' # relax the parallel regression assumption
#' ( fit_orig <- ordinal::clm(
#'   Sat ~ 1, data = house_data,
#'   nominal = ~ Infl + Cont
#' ) )
#' ( fit_wrap <- clm_wrapper(
#'   Sat ~ Infl + Cont, data = house_data,
#'   parallel_reg = FALSE
#' ) )
#' @export
clm_wrapper <- function(
  formula,
  data,
  weights = NULL,
  parallel_reg = NULL,
  link = NULL,
  threshold = NULL,
  ...,
  call = rlang::caller_env()
) {
  rlang::check_installed("ordinal")

  # capture before forcing the promises, so that the rebuilt call below records
  # engine arguments as the user wrote them rather than as their values
  arg_exprs <- rlang::enexprs(link = link, threshold = threshold)
  dot_exprs <- rlang::enexprs(...)
  dots <- list(...)

  formulas <- if (isFALSE(parallel_reg)) {
    clm_formulas(formula, data, call = call)
  } else {
    list(formula = formula)
  }

  args <- c(formulas, list(data = data))
  if (! is.null(weights)) {
    args$weights <- weights
  }
  if (! is.null(link)) {
    if (link == "logistic") link <- "logit"
    args$link <- link
  }
  if (! is.null(threshold)) {
    threshold <- switch(
      threshold,
      flexible = "flexible",
      symmetric_median = "symmetric",
      symmetric_zero = "symmetric2",
      equidistant = "equidistant",
      threshold
    )
    args$threshold <- threshold
  }
  args <- c(args, dots)

  res <- do.call(ordinal::clm, args)

  # `do.call()` inlines the data into the recorded call. Rebuild it so that
  # printing the fit shows the formulas and engine arguments that were used.
  res$call <- rlang::call2(
    "clm",
    !!! formulas,
    data = rlang::sym("data"),
    !!! arg_exprs,
    !!! dot_exprs,
    .ns = "ordinal"
  )

  res
}

# Split a model formula into the location and `nominal` formulas that
# `ordinal::clm()` expects when the parallel regression assumption is relaxed.
# Every predictor moves to `nominal`, leaving an intercept-only location
# formula, so that `clm()` has no aliased coefficients to drop. An offset has no
# coefficient to vary across thresholds, so it stays in the location formula.
clm_formulas <- function(formula, data, call = rlang::caller_env()) {
  term_info <- stats::terms(formula, data = data)
  term_labels <- attr(term_info, "term.labels")
  offset <- attr(term_info, "offset")

  if (length(term_labels) == 0L) {
    cli::cli_abort(
      "{.code parallel_reg = FALSE} needs at least one predictor to make
       non-parallel, but {.arg formula} has none.",
      call = call
    )
  }

  loc <- formula
  loc[[3]] <- if (length(offset)) {
    as.list(attr(term_info, "variables"))[[offset + 1L]]
  } else {
    1
  }

  nominal <- stats::reformulate(term_labels, env = rlang::f_env(formula))

  list(formula = loc, nominal = nominal)
}
