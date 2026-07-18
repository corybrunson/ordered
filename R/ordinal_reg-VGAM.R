#' Wrappers for `VGAM`
#'
#' These wrappers convert the standardized `odds_link` options encoded in
#' [`dials::values_odds_link`] to [`VGAM::vglmff-class`] objects passed to the
#' `family` argument of [VGAM::vglm()] and [VGAM::vgam()].
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
#' @param Thresh The threshold constraints. See [dials::threshold_structure()]
#'   for details.
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
#'   family = "stopping_ratio", link = "probit", Thresh = "symmetric_zero",
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
    parallel_reg = NULL,
    Thresh = NULL,
    ...
) {
  rlang::check_installed("VGAM")

  if (! is.null(parallel_reg)) {
    parallel <- parallel_reg_to_vglm_parallel(parallel_reg, formula)
  }

  # `parallel` can be logical or formula (VGAM supports both)
  if (! is.logical(parallel) && ! inherits(parallel, "formula")) {
    stop_input_type(parallel, "a logical vector or formula")
  }

  # TODO: Ensure that `formula = cbind(...) ~ ...` is disallowed, for this and
  # for other `ordinal_reg()` engines.

  # match and convert model arguments
  family <- match_VGAM_family(family)
  link <- match_VGAM_link(link)
  Thresh <- match_VGAM_Thresh(Thresh)

  # `acat()` does not support certain link functions
  if (family == "acat" &&
      link %in% c("logitlink", "probitlink", "clogloglink")) {
    cli::cli_abort(
      c(
        "The {.val adjacent_categories} family is not compatible with
         the {.val {link}} link function.",
        "i" = "Use {.val cauchitlink} or {.val identitylink} instead."
      )
    )
  }

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
    family = "cumulative_link", link = "logistic",
    parallel = TRUE,
    parallel_reg = NULL,
    Thresh = NULL,
    ...
) {
  rlang::check_installed("VGAM")

  if (! is.null(parallel_reg)) {
    parallel <- parallel_reg_to_vglm_parallel(parallel_reg, formula)
  }
  # `parallel` can be logical or formula (VGAM supports both)
  if (! is.logical(parallel) && ! inherits(parallel, "formula")) {
    stop_input_type(parallel, "a logical vector or formula")
  }

  # match and convert model arguments
  family <- match_VGAM_family(family)
  link <- match_VGAM_link(link)
  Thresh <- match_VGAM_Thresh(Thresh)

  # `acat()` does not support certain link functions
  if (family == "acat" &&
      link %in% c("logitlink", "probitlink", "clogloglink")) {
    cli::cli_abort(
      c(
        "The {.val adjacent_categories} family is not compatible with
         the {.val {link}} link function.",
        "i" = "Use {.val cauchitlink} or {.val identitylink} instead."
      )
    )
  }

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

match_VGAM_Thresh <- function(Thresh) {
  if (is.null(Thresh)) {
    return(NULL)
  }
  Thresh <- match.arg(Thresh, values_threshold_structure_VGAM)
  switch(
    Thresh,
    flexible = "free",
    symmetric_median = "symm1",
    symmetric_zero = "symm0",
    equidistant = "equid",
    qnorm = "qnorm"
  )
}

predict_VGAM_class_post <- function(x, object) {
  x <- apply(x, 1L, which.max)
  x <- ordered(object$lvl[x], object$lvl)
  tibble::tibble(.pred_class = x)
}

predict_VGAM_prob_post <- function(x, object) {
  colnames(x) <- object$lvl
  tibble::as_tibble(x)
}

# ------------------------------------------------------------------------------

#' Translate `parallel_reg` to VGAM `parallel` argument
#'
#' @param parallel_reg A parallel regression specification.
#' @param formula The model formula (used to extract predictor names for list
#'   forms).
#' @keywords internal
#' @returns A logical or formula suitable for the `parallel` argument of VGAM
#'   family functions.
parallel_reg_to_vglm_parallel <- function(parallel_reg, formula) {
  # single logical
  if (is.logical(parallel_reg) && length(parallel_reg) == 1L) {
    return(parallel_reg)
  }

  # single formula
  # TRUE  ~ vars → TRUE  ~ -1 + vars
  # FALSE ~ vars → FALSE ~  1 + vars
  if (inherits(parallel_reg, "formula")) {
    return(formula_to_vglm_parallel(parallel_reg))
  }

  # list
  if (is.list(parallel_reg)) {
    return(list_to_vglm_parallel(parallel_reg, formula))
  }

  cli::cli_abort("Invalid {.arg parallel_reg} specification.")
}

#' @keywords internal
formula_to_vglm_parallel <- function(pr_formula) {
  lhs <- pr_formula[[2L]]
  rhs_vars <- all.vars(pr_formula[[3L]])

  if (isTRUE(lhs)) {
    # RHS names parallel terms; VGAM: TRUE ~ -1 + vars
    rhs <- paste("-1 +", paste(rhs_vars, collapse = " + "))
  } else {
    # RHS names non-parallel terms; VGAM: FALSE ~ -1 + vars
    rhs <- paste("-1 +", paste(rhs_vars, collapse = " + "))
  }
  as.formula(paste(format(lhs), "~", rhs))
}

#' @keywords internal
list_to_vglm_parallel <- function(lst, formula) {
  parallel_vars <- character(0L)
  nonparallel_vars <- character(0L)
  has_bare_logical <- FALSE

  for (el in lst) {
    if (is.logical(el) && length(el) == 1L) {
      has_bare_logical <- TRUE
      if (isTRUE(el)) {
        parallel_vars <- all.vars(formula[[3L]])
      }
      # bare FALSE: no action needed (non-parallel is VGAM default)
    } else if (inherits(el, "formula")) {
      lhs <- el[[2L]]
      rhs_vars <- all.vars(el[[3L]])
      if (isTRUE(lhs)) {
        parallel_vars <- union(parallel_vars, rhs_vars)
      } else {
        nonparallel_vars <- union(nonparallel_vars, rhs_vars)
      }
    }
  }

  # check overlap
  overlap <- intersect(parallel_vars, nonparallel_vars)
  if (length(overlap) > 0L) {
    cli::cli_abort(
      "Variable{?s} {.val {overlap}} appear{?s/} in both parallel and
      non-parallel specifications."
    )
  }

  # when no bare logical is present, every predictor must appear
  # in at least one formula entry
  if (!has_bare_logical) {
    all_pred_vars <- all.vars(formula[[3L]])
    covered <- union(parallel_vars, nonparallel_vars)
    missing <- setdiff(all_pred_vars, covered)
    if (length(missing) > 0L) {
      cli::cli_abort(
        "Variable{?s} {.val {missing}} not specified in {.arg parallel_reg}
        list. Either use a bare logical ({.val TRUE}/{.val FALSE}) to set
        defaults, or include all predictors in the formula entries."
      )
    }
  }

  if (length(parallel_vars) > 0L) {
    return(as.formula(paste(
      "TRUE ~ -1 +", paste(parallel_vars, collapse = " + ")
    )))
  }

  return(FALSE)
}
