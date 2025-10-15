#' Wrappers for `VGAM`
#'
#' These wrappers convert the standardized `odds_link` options encoded in
#' [`dials::values_odds_link`] to [`VGAM::vglmff-class`] objects passed to the
#' `family` argument of [VGAM::vglm()] and [VGAM::vgam()].
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param ... Additional arguments to pass.
#' @details Note that `VGAM::vglm()` and `VGAM::vgam()` treat the rows of `data`
#'   as the units of observation: Compressed `data` with one row per predictor
#'   set will be understood as having a multinomial-valued outcome; only
#'   expanded data with one row per case will be understood as having a
#'   single-valued ordinal outcome. (This divide cannot be bridged by passing a
#'   column of counts to the `weights` argument.) These wrappers require a
#'   single ordinal outcome column and therefore do not accept the convenient
#'   `cbind(y1, y2, ...) ~ x1 + x2 + ...` encoding commonly used in `VGAM`.
#' @keywords internal
#' @examples
#' values_ordinal_link_VGAM
#' dials::ordinal_link(values = values_ordinal_link_VGAM)
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("VGAM")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # fit wrapper for linear model
#' ( fit_orig <- VGAM::vglm(
#'   Sat ~ Type + Infl + Cont,
#'   family = VGAM::sratio(link = "probitlink", parallel = TRUE),
#'   data = house_data
#' ) )
#' ( fit_wrap <- VGAM_vglm_wrapper(
#'   Sat ~ Type + Infl + Cont,
#'   family = "stopping_ratio", link = "probit",
#'   data = house_data
#' ) )
#' # fit wrapper for additive model
#' ( fit_orig <- VGAM::vgam(
#'   Sat ~ Type + Infl + Cont,
#'   family = VGAM::cratio(link = "clogloglink", parallel = TRUE),
#'   data = house_data
#' ) )
#' ( fit_wrap <- VGAM_vgam_wrapper(
#'   Sat ~ Type + Infl + Cont,
#'   family = "continuation_ratio", link = "cloglog",
#'   data = house_data
#' ) )
#' @export
VGAM_vglm_wrapper <- function(
    formula, data,
    family = "cumulative_link", link = "logistic",
    parallel = TRUE,
    ...
) {
  rlang::check_installed("VGAM")

  # for now, require `parallel` to be logical
  check_logical(parallel)

  # TODO: Ensure that `formula = cbind(...) ~ ...` is disallowed, for this and
  # for other `ordinal_reg()` engines.

  # match and convert odds link options
  family <- match_VGAM_family(family)
  link <- match_VGAM_link(link)

  # execute nested call on modified inputs
  family_call <- rlang::call2(
    .fn = family, .ns = "VGAM",
    link = link, parallel = parallel
  )
  cl <- rlang::call2(
    .fn = "vglm", .ns = "VGAM",
    formula = expr(formula), data = expr(data),
    family = family_call,
    ...
  )
  rlang::eval_tidy(cl)
}

# REVIEW: Reduce these to one wrapper function with a `fun = "vglm"` argument?

#' @rdname VGAM_vglm_wrapper
#' @export
VGAM_vgam_wrapper <- function(
    formula, data,
    family = "cumulative_link", link = "logistic",
    parallel = TRUE,
    ...
) {
  rlang::check_installed("VGAM")

  # for now, require `parallel` to be logical
  check_logical(parallel)

  # match and convert odds link options
  family <- match_VGAM_family(family)
  link <- match_VGAM_link(link)

  # execute nested call on modified inputs
  family_call <- rlang::call2(
    .fn = family, .ns = "VGAM",
    link = link, parallel = parallel
  )
  cl <- rlang::call2(
    .fn = "vgam", .ns = "VGAM",
    formula = expr(formula), data = expr(data),
    family = family_call,
    ...
  )
  rlang::eval_tidy(cl)
}

#' @rdname VGAM_vglm_wrapper
#' @export
values_ordinal_link_VGAM <- c(
  dials::values_ordinal_link,
  # TODO: Expand to include link functions to other domains than [0,1].
  c("foldsqrt", "logc", "gord", "pord", "nbord")
)

match_VGAM_family <- function(family) {
  family <- match.arg(family, dials::values_odds_link)
  switch(
    family,
    cumulative_link = "cumulative",
    adjacent_categories = "acat",
    continuation_ratio = "cratio",
    stopping_ratio = "sratio"
  )
}

match_VGAM_link <- function(link) {
  link <- match.arg(link, values_ordinal_link_VGAM)
  # REVIEW: Change `logistic` to `logit` in {dials}?
  if (link == "logistic") link <- "logit"
  if (link == "loglog") {
    cli::cli_abort(
      c(
        "The `vglm` engine does not support the log-log ordinal link.",
        "i" = "See `?VGAM::Links` for provided link functions."
      )
    )
  }
  paste0(link, "link")
}

predict_VGAM_class_post <- function(x, object) {
  x <- apply(x, 1L, which.max)
  x <- ordered(object$lvl[x], object$lvl)
  tibble::tibble(.pred_class = x)
}

predict_VGAM_prob_post <- function(x, object) {
  tibble::as_tibble(x)
}
