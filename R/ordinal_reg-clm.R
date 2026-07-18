#' Wrapper for `clm`
#'
#' Translates the `parallel_reg` specification to the `nominal` formula accepted
#' by [ordinal::clm()] and validates that each predictor is assigned to exactly
#' one side of the parallel regression specification.
#' @param formula The model formula.
#' @param data The data frame.
#' @param parallel_reg A parallel regression specification. See
#'   [parsnip::ordinal_reg()] for details.
#' @param ... Additional arguments passed to [ordinal::clm()].
#' @keywords internal
#' @returns An object of class `clm` as returned by [ordinal::clm()].

#' @examplesIf rlang::is_installed("MASS") && rlang::is_installed("ordinal")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' clm_wrapper(
#'   Sat ~ Infl + Cont, data = house_data,
#'   parallel_reg = FALSE
#' )
#' clm_wrapper(
#'   Sat ~ Infl + Cont, data = house_data,
#'   parallel_reg = FALSE ~ Infl + Cont
#' )
#' ordinal::clm(
#'   Sat ~ Infl + Cont, data = house_data,
#'   nominal = ~ Infl + Cont
#' )
#' @export
clm_wrapper <- function(
    formula, data,
    parallel_reg = NULL,
    ...
) {
  rlang::check_installed("ordinal")

  nominal <- NULL
  if (!is.null(parallel_reg)) {
    nominal <- parallel_reg_to_clm_nominal(parallel_reg, formula)
  }

  cl <- rlang::call2(
    "clm", .ns = "ordinal",
    formula = rlang::expr(formula),
    data = rlang::expr(data),
    nominal = nominal,
    ...
  )
  rlang::eval_tidy(cl)
}

#' Translate `parallel_reg` to clm `nominal` formula
#'
#' @param parallel_reg A parallel regression specification.
#' @param formula The model formula (used to extract predictor names).
#' @keywords internal
#' @returns A formula suitable for the `nominal` argument of [ordinal::clm()],
#'   or `NULL` for proportional odds.
parallel_reg_to_clm_nominal <- function(parallel_reg, formula) {
  # single logical
  if (is.logical(parallel_reg) && length(parallel_reg) == 1L) {
    if (isTRUE(parallel_reg)) {
      return(NULL)
    }
    # FALSE: all non-parallel
    pred_vars <- all.vars(formula[[3L]])
    return(as.formula(paste("~", paste(pred_vars, collapse = " + "))))
  }

  # single formula
  if (inherits(parallel_reg, "formula")) {
    return(formula_to_clm_nominal(parallel_reg, formula))
  }

  # list
  if (is.list(parallel_reg)) {
    return(list_to_clm_nominal(parallel_reg, formula))
  }

  cli::cli_abort("Invalid {.arg parallel_reg} specification.")
}

#' @keywords internal
formula_to_clm_nominal <- function(pr_formula, model_formula) {
  lhs <- pr_formula[[2L]]
  rhs_vars <- all.vars(pr_formula[[3L]])

  if (isFALSE(lhs)) {
    # RHS names non-parallel terms
    if (length(rhs_vars) == 0L) {
      return(NULL)
    }
    return(as.formula(paste("~", paste(rhs_vars, collapse = " + "))))
  }

  # TRUE: RHS names parallel terms; non-parallel = complement
  all_pred_vars <- all.vars(model_formula[[3L]])
  nonparallel <- setdiff(all_pred_vars, rhs_vars)
  if (length(nonparallel) == 0L) {
    return(NULL)
  }
  return(as.formula(paste("~", paste(nonparallel, collapse = " + "))))
}

#' @keywords internal
list_to_clm_nominal <- function(lst, model_formula) {
  parallel_vars <- character(0L)
  nonparallel_vars <- character(0L)
  has_bare_logical <- FALSE

  for (el in lst) {
    if (is.logical(el) && length(el) == 1L) {
      has_bare_logical <- TRUE
      if (isTRUE(el)) {
        parallel_vars <- all.vars(model_formula[[3L]])
      } else {
        nonparallel_vars <- all.vars(model_formula[[3L]])
      }
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

  # check overlap between explicit formula entries
  overlap <- intersect(parallel_vars, nonparallel_vars)
  if (length(overlap) > 0L) {
    cli::cli_abort(
      "Variable{?s} {.val {overlap}} appear{?s/} in both parallel and
      non-parallel specifications. The {.code clm} engine must treat each
      predictor as either parallel regression or category-specific (not both)."
    )
  }

  # when no bare logical is present, every predictor must appear
  # in at least one formula entry
  if (!has_bare_logical) {
    all_pred_vars <- all.vars(model_formula[[3L]])
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

  # for clm: nonparallel vars go in nominal
  all_pred_vars <- all.vars(model_formula[[3L]])

  if (length(nonparallel_vars) > 0L) {
    # if parallel vars were also specified (via "all" or formulas),
    # the nonparallel vars are the ones to put in nominal
    if (length(parallel_vars) > 0L &&
        !identical(sort(parallel_vars), sort(all_pred_vars))) {
      # both sides specified explicitly: use nonparallel vars
      return(as.formula(paste("~", paste(nonparallel_vars, collapse = " + "))))
    }
    # only nonparallel specified (or parallel = "all"): use nonparallel vars
    return(as.formula(paste("~", paste(nonparallel_vars, collapse = " + "))))
  }

  if (length(parallel_vars) > 0L) {
    # only parallel specified: nonparallel = complement
    nonparallel <- setdiff(all_pred_vars, parallel_vars)
    if (length(nonparallel) == 0L) {
      return(NULL)
    }
    return(as.formula(paste("~", paste(nonparallel, collapse = " + "))))
  }

  # neither specified: default PO
  return(NULL)
}
