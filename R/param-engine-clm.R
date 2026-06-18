#' Dials for `ordinal` engine parameters
#'
#' The `threshold_structure` dial is auxiliary to ordinal regression models that
#' use the "clm" engine. It corresponds to the `threshold` tuning parameter that
#' would be specified using `set_engine("clm", ...)`.
#'
#' The vector `values_ordinal_link_clm` extends the default `ordinal_link`
#' options encoded in [`dials::values_ordinal_link`] to those accepted by
#' [`ordinal::clm()`].
#'
#' @name clm_parameters
#' @param values A character string of possible values.
#' @details `threshold_structure()` is a dial for the threshold structure in
#'   cumulative link models. See `?ordinal::clm` for more details. Use
#'   `set_args(threshold = ...)` to set this parameter on a model spec. These
#'   parameters are engine-specific, not general to ordinal regression models,
#'   so are provided here rather than in `dials`.
#' @returns An object of S3 parent class `param` and primary class `qual_param`;
#'   see [dials::new_qual_param()].
#' @examples
#' values_ordinal_link_clm
#' dials::ordinal_link(values = values_ordinal_link_clm)
#' threshold_structure()
#'
#' @export
#' @rdname clm_parameters
threshold_structure <- function(
    values = c("flexible", "symmetric", "symmetric2", "equidistant")
) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(threshold_structure = "Threshold Structure"),
    finalize = NULL
  )
}

#' @rdname clm_parameters
#' @export
values_ordinal_link_clm <- c(
  dials::values_ordinal_link,
  c("Aranda-Ordaz", "log-gamma")
)
