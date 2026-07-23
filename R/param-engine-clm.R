#' Dials for `ordinal` engine parameters
#'
#' The vector `values_ordinal_link_clm` extends the default `ordinal_link`
#' options encoded in [`dials::values_ordinal_link`] to those accepted by
#' [`ordinal::clm()`].
#'
#' The `threshold_structure` dial is defined in the \pkg{dials} package.
#' See `?dials::threshold_structure` for details.
#'
#' @name clm_parameters
#' @returns An object of S3 parent class `param` and primary class `qual_param`;
#'   see [dials::new_qual_param()].
#' @examples
#' values_ordinal_link_clm
#' dials::ordinal_link(values = values_ordinal_link_clm)
#' dials::threshold_structure(values = dials::values_threshold_structure)
#' @keywords internal
NULL

#' @rdname clm_parameters
#' @export
values_ordinal_link_clm <- c(
  dials::values_ordinal_link,
  c("Aranda-Ordaz", "log-gamma")
)
