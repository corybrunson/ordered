#' Dials for `ordinalNet` engine parameters
#'
#' These parameters are auxiliary to ordinal regression models that use the
#' "ordinalNet" engine. They correspond to tuning parameters that would be
#' specified using `set_engine("ordinalNet", ...)`.
#'
#' @name ordinalNet_parameters
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively. If a transformation is specified,
#'   these values should be in the _transformed units_.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::transform_log10()` or `scales::transform_reciprocal()`. If not
#'   provided, the default is used which matches the units used in `range`. If
#'   no transformation, `NULL`.
#'
#' @details These functions generate parameters for [parsnip::ordinal_reg()]
#'   models using the `"ordinalNet"` engine. See `?ordinalNet::ordinalNet()` for
#'   more details on the original parameters. These parameters are
#'   engine-specific, not general to ordinal regression models, so are provided
#'   here rather than in `dials`.
#'
#' * `parallel_penalty_factor()`: A nonnegative numeric factor that scales the
#'   penalty on all parallel terms; tunes `parallelPenaltyFactor`. Only used
#'   when `parallelTerms = TRUE`.
#'
#' @returns An object of S3 parent class `param` and primary class
#'   `quant_param`; see [dials::new_quant_param()].
#' @seealso [dials::penalty()]
#' @examples
#' parallel_penalty_factor()
#'
#' @export
#' @rdname ordinalNet_parameters
parallel_penalty_factor <- function(
    range = c(-Inf, Inf),
    trans = scales::transform_log10()
) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, FALSE),
    trans = trans,
    label = c(
      parallel_penalty_factor = "Parallel Penalty Factor"
    ),
    finalize = NULL
  )
}
