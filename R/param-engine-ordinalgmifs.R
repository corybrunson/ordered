#' Dials for `ordinalgmifs` engine parameters
#'
#' The vectors `values_*_ordinalgmifs` extend and restrict the default options
#' encoded in [`dials::values_ordinal_link`] and [`dials::values_odds_link`] to
#' those accepted by [`ordinalgmifs::ordinalgmifs()`].
#'
#' @name ordinalgmifs_parameters
#' @returns An object of S3 parent class `param` and primary class `qual_param`;
#'   see [dials::new_qual_param()].
#' @examples
#' values_odds_link_ordinalgmifs
#' dials::odds_link(values = values_odds_link_ordinalgmifs)
#' values_ordinal_link_ordinalgmifs
#' dials::ordinal_link(values = values_ordinal_link_ordinalgmifs)
NULL

#' @rdname ordinalgmifs_parameters
#' @export
values_odds_link_ordinalgmifs <- c(
  setdiff(dials::values_odds_link, "continuation_ratio"),
  "stereotype"
)

#' @rdname ordinalgmifs_parameters
#' @export
values_ordinal_link_ordinalgmifs <- c(
  "logistic",
  "probit",
  "cloglog",
  "loge"
)
