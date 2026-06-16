#' Dials for `orf` engine parameters
#'
#' These parameters are auxiliary to random forest models that use the
#' "orf" engine. They correspond to tuning parameters that would be
#' specified using `set_engine("orf", ...)`.
#'
#' @name orf_parameters
#' @param values A logical vector of possible values. See `values_ord_metric`.
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively. If a transformation is specified,
#'   these values should be in the _transformed units_.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::transform_log10()` or `scales::transform_reciprocal()`. If not
#'   provided, the default is used which matches the units used in `range`. If
#'   no transformation, `NULL`.
#'
#' @details These functions generate parameters for [parsnip::rand_forest()]
#'   models using the `"orf"` engine. See `?orf::orf()` for more details on
#'   the original parameters. These parameters are engine-specific, not general
#'   to random forest models, so are provided here rather than in `dials`.
#'
#' * `sample_fraction()`: The fraction of observations to be subsampled;
#'   tunes `sample.fraction`.
#'
#' * `honesty()`: Whether to use honest splitting (sample splitting);
#'   tunes `honesty`.
#'
#' * `honesty_fraction()`: The fraction of observations reserved for the
#'   honest (estimation) sample; tunes `honesty.fraction`.
#'
#' @returns An object of S3 parent class `param` and primary class `qual_param`
#'   or `quant_param`; see [dials::new_qual_param()] and
#'   [dials::new_quant_param()].
#' @seealso [dials::trees()]
#' @examples
#' sample_fraction()
#' honesty()
#' honesty_fraction()
#'

#' @export
#' @rdname orf_parameters
sample_fraction <- function(range = c(0.1, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, TRUE),
    trans = trans,
    label = c(sample_fraction = "Subsampling Fraction"),
    finalize = NULL
  )
}

#' @export
#' @rdname orf_parameters
honesty <- function(values = c(TRUE, FALSE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(honesty = "Honest Splitting?"),
    finalize = NULL
  )
}

#' @export
#' @rdname orf_parameters
honesty_fraction <- function(range = c(0.1, 0.9), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, FALSE),
    trans = trans,
    label = c(honesty_fraction = "Honest Sample Fraction"),
    finalize = NULL
  )
}
