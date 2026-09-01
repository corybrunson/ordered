#' Fit wrappers for `VGAM`
#'
#' These wrappers reorganize arguments into proper calls to [VGAM::vglm()] and
#' [VGAM::vgam()], which specialize to several families of ordinal regression
#' models. The standardized argument values used by `parsnip::ordinal_reg()` and
#' `parsnip::gen_additive_mod()` are matched and converted to the values native
#' to `VGAM` at translation time.
#'
#' The vector `values_ordinal_link_VGAM` extends the default `ordinal_link`
#' options encoded in [`dials::values_ordinal_link`] to those accepted by
#' [`VGAM::vglm()`] and [`VGAM::vgam()`].
#'
#' The vector `values_threshold_structure_VGAM` extends the default
#' `threshold_structure` options encoded in
#' [`dials::values_threshold_structure`] to those accepted by [`VGAM::vglm()`]
#' and [`VGAM::vgam()`].
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param family The `VGAM` family function, as a character string (e.g.
#'   `"cumulative"`, `"acat"`, `"cratio"`, `"sratio"`).
#' @param link The `VGAM` link function (e.g. `"logitlink"`).
#' @param parallel Logical; whether predictor effects are shared across
#'   thresholds. It corresponds to the standardized `parallel_reg` argument (see
#'   [dials::parallel_reg()]).
#' @param Thresh Character; the threshold constraint pattern. It corresponds
#'   to the standardized `threshold_structure` argument (see
#'   [dials::threshold_structure()]).
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
#' @returns An object of S3 parent class `VGAM` and primary classes `vglm` or
#'   `vgam` as returned by [VGAM::vglm()] and [VGAM::vgam()].
#' @examples
#' values_ordinal_link_VGAM
#' dials::ordinal_link(values = values_ordinal_link_VGAM)
#' values_threshold_structure_VGAM
#' dials::threshold_structure(values = values_threshold_structure_VGAM)
#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("VGAM")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # fit wrapper for linear model
#' ( fit_orig <- VGAM::vglm(
#'   Sat ~ Type + Infl + Cont,
#'   family = VGAM::sratio(
#'     link = "probitlink", parallel = TRUE, Thresh = "symm0"
#'   ),
#'   data = house_data
#' ) )
#' ( fit_wrap <- VGAM_vglm_wrapper(
#'   Sat ~ Type + Infl + Cont,
#'   family = "sratio",
#'   link = "probitlink", parallel = TRUE, Thresh = "symm0",
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
#'   family = "cratio", link = "clogloglink", parallel = TRUE,
#'   data = house_data
#' ) )
#' @export
VGAM_vglm_wrapper <- function(
    formula, data,
    family = "cumulative",
    link = "logitlink",
    parallel = FALSE,
    Thresh = NULL,
    ...
) {
  rlang::check_installed("VGAM")

  # TODO: Ensure that `formula = cbind(...) ~ ...` is disallowed, for this and
  # for other `ordinal_reg()` engines.

  # execute nested call on modified inputs
  family_call <- rlang::call2(
    .fn = family, .ns = "VGAM",
    link = link, parallel = parallel, Thresh = Thresh
  )
  cl <- rlang::call2(
    .fn = "vglm", .ns = "VGAM",
    formula = rlang::expr(formula), data = rlang::expr(data),
    family = family_call,
    ...
  )
  suppressWarnings(rlang::eval_tidy(cl))
}

#' @rdname VGAM_vglm_wrapper
#' @export
VGAM_vgam_wrapper <- function(
    formula, data,
    family = "cumulative",
    link = "logitlink",
    parallel = FALSE,
    Thresh = NULL,
    ...
) {
  rlang::check_installed("VGAM")

  # execute nested call on modified inputs
  family_call <- rlang::call2(
    .fn = family, .ns = "VGAM",
    link = link, parallel = parallel, Thresh = Thresh
  )
  cl <- rlang::call2(
    .fn = "vgam", .ns = "VGAM",
    formula = rlang::expr(formula), data = rlang::expr(data),
    family = family_call,
    ...
  )
  suppressWarnings(rlang::eval_tidy(cl))
}

#' @rdname VGAM_vglm_wrapper
#' @export
values_ordinal_link_VGAM <- c(
  dials::values_ordinal_link,
  # TODO: Expand to include link functions to other domains than [0,1].
  c("foldsqrt", "logc", "gord", "pord", "nbord")
)

#' @rdname VGAM_vglm_wrapper
#' @export
values_threshold_structure_VGAM <- c(
  dials::values_threshold_structure,
  "qnorm"
)

predict_VGAM_class_post <- function(x, object) {
  x <- apply(x, 1L, which.max)
  x <- ordered(object$lvl[x], object$lvl)
  tibble::tibble(.pred_class = x)
}

predict_VGAM_prob_post <- function(x, object) {
  colnames(x) <- object$lvl
  tibble::as_tibble(x)
}
