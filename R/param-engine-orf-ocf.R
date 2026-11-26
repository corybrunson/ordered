#' Dials for `orf` and `ocf` engine parameters
#'
#' These parameters are auxiliary to random forest models that use the "orf" or
#' "ocf" engines. They correspond to tuning parameters that would be specified
#' using `set_engine("orf", ...)` or `set_engine("ocf", ...)`.
#'
#' @name orf_parameters
#' @aliases ocf_parameters
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
#'   models using the `"orf"` and `"ocf"` engines. See `?orf::orf()` and
#'   `?ocf::ocf()` for more details on the original parameters. These parameters
#'   are engine-specific, not general to random forest models, so are provided
#'   here rather than in `dials`.
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
#' * `alpha_balance()`: The split balance parameter for ordered correlation
#'   forests; tunes `alpha`, which ranges from 0 to 0.5 (`"ocf"` only).
#'
#' * `replace_samples()`: Whether to sample with replacement; tunes `replace`
#'    (`"ocf"` only).
#'
#' The `max.depth` argument is tuned with [dials::tree_depth()] (`"ocf"` only).
#'
#' @returns An object of S3 parent class `param` and primary class `qual_param`
#'   or `quant_param`; see [dials::new_qual_param()] and
#'   [dials::new_quant_param()].
#' @seealso [dials::trees()]
#' @examples
#' sample_fraction()
#' honesty()
#' honesty_fraction()
#' alpha_balance()
#' replace_samples()
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

#' @export
#' @rdname orf_parameters
alpha_balance <- function(range = c(0, 0.5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(alpha_balance = "Balance Parameter"),
    finalize = NULL
  )
}

#' @export
#' @rdname orf_parameters
replace_samples <- function(values = c(FALSE, TRUE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(replace_samples = "Sample With Replacement?"),
    finalize = NULL
  )
}
