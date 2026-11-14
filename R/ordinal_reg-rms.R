#' Fit and predict wrappers for `lrm` and `orm`
#'
#' `rms::orm()` did not support case weights or `penalty` before version 7.0-0.
#' The fit wrapper errs or warns informatively when either is requested from an
#' older version. Both `rms::lrm()` and `rms::orm()` use [rms::predictrms()]
#' internally, which does not accept tibble inputs. The predict wrapper ensures
#' that `newdata` is converted to a plain data frame before the S3 method is
#' dispatched.
#'
#' @param formula The formula to pass.
#' @param data The data frame to pass.
#' @param weights An optional vector of case weights to pass.
#' @param call The execution environment of a currently running function.
#' @param object A fitted model object of class `"lrm"` or `"orm"`.
#' @param newdata A data frame or tibble of predictors.
#' @param type The prediction type, passed to the underlying method.
#' @param ... Additional arguments passed to the underlying method.
#' @keywords internal
#' @returns `rms_orm_wrapper()` returns an object of class `orm` as returned by
#'   [rms::orm()]. `predict_lrm_wrapper()` returns the result of
#'   [stats::predict()] dispatched on `object`.

#' @examplesIf rlang::is_installed("rms (>= 7.0.0)") && rlang::is_installed("MASS")
#' house_data <-
#'   MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
#' # fit wrapper
#' ( fit_orig <- rms::orm(Sat ~ Infl + Type + Cont, data = house_data) )
#' ( fit_wrap <- rms_orm_wrapper(
#'   Sat ~ Infl + Type + Cont, data = house_data
#' ) )
#' # predict wrapper
#' ( fit <- rms::lrm(Sat ~ Infl + Type + Cont, data = house_data) )
#' predict(
#'   fit,
#'   newdata = head(house_data, 3),
#'   type = "fitted.ind"
#' )
#' # tibble input is converted internally
#' predict_lrm_wrapper(
#'   fit,
#'   newdata = tibble::as_tibble(head(house_data, 3)),
#'   type = "fitted.ind"
#' )
#' @export
rms_orm_wrapper <- function(
  formula,
  data,
  weights = NULL,
  ...,
  call = rlang::caller_env()
) {
  rlang::check_installed("rms")

  weights_expr <- rlang::enexpr(weights)
  dot_exprs <- rlang::enexprs(...)
  dots <- list(...)

  rms_ver <- rms_version()
  rms_ver_chr <- as.character(rms_ver)
  old_rms <- rms_ver < package_version("7.0-0")
  penalty <- dots$penalty
  zero_penalty <-
    ! is.null(penalty) &&
    is.numeric(penalty) &&
    length(penalty) == 1L &&
    isTRUE(penalty == 0)

  if (old_rms && ! is.null(weights)) {
    cli::cli_abort(
      c(
        paste0(
          "The {.val orm} engine does not support case weights with ",
          "{.pkg rms} {.val {rms_ver_chr}}."
        ),
        "i" = "Use {.pkg rms} >= 7.0-0 or omit {.arg case_weights}."
      ),
      call = call
    )
  }

  if (old_rms && ! is.null(penalty) && ! zero_penalty) {
    cli::cli_abort(
      c(
        paste0(
          "The {.val orm} engine does not support a nonzero {.arg penalty} ",
          "with {.pkg rms} {.val {rms_ver_chr}}."
        ),
        "i" = "Use {.pkg rms} >= 7.0-0 or omit {.arg penalty}."
      ),
      call = call
    )
  }

  if (old_rms && zero_penalty) {
    cli::cli_warn(
      c(
        paste0(
          "The {.arg penalty} argument was set to 0, which is not supported ",
          "by {.pkg rms} {.val {rms_ver_chr}} for the {.val orm} engine."
        ),
        "i" = "The argument will be omitted."
      ),
      call = call
    )
  }

  if (old_rms) {
    dots$penalty <- NULL
    dot_exprs$penalty <- NULL
  }

  args <- c(list(formula = formula, data = data), dots)
  if (! is.null(weights)) {
    args$weights <- weights
    dot_exprs$weights <- weights_expr
  }

  res <- do.call(rms::orm, args)

  res$call <- rlang::call2(
    "orm",
    formula = formula,
    data = rlang::sym("data"),
    !!! dot_exprs,
    .ns = "rms"
  )

  res
}

rms_version <- function() {
  utils::packageVersion("rms")
}

#' @rdname rms_orm_wrapper
#' @export
predict_lrm_wrapper <- function(object, newdata, type, ...) {
  if (inherits(newdata, "tbl_df")) {
    newdata <- as.data.frame(newdata)
  }
  stats::predict(object, newdata = newdata, type = type, ...)
}
