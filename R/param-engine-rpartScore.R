#' Dials for `rpartScore` engine parameters
#'
#' These parameters are auxiliary to decision tree models that use the
#' "rpartScore" engine. They correspond to tuning parameters that would be
#' specified using `set_engine("rpartScore", ...)`.
#'
#' @name rpartScore_parameters
#' @param values A character string of possible values.
#' @details `split_func` and `prune_func` are dials for `split` and `prune`,
#'   respectively. See `?rpartScore::rpartScore` for more details on the
#'   original parameters. These parameters are engine-specific, not general to
#'   decision tree models, so are provided here rather than in `dials`.
#' @examples
#' split_func()
#' prune_func()

#' @export
#' @rdname rpartScore_parameters
split_func <- function(values = c("abs", "quad")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(split_func = "Splitting Function"),
    finalize = NULL
  )
}

#' @export
#' @rdname rpartScore_parameters
prune_func <- function(values = c("mr", "mc")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(prune_func = "Pruning Function"),
    finalize = NULL
  )
}
