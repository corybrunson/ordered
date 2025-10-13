#' Dials for `ordinalForest` engine parameters
#'
#' These parameters are auxiliary to random forest models that use the
#' "ordinalForest" engine. They correspond to tuning parameters that would be
#' specified using `set_engine("ordinalForest", ...)`.
#'
#' @name ordinalForest_parameters
#' @param values A character string of possible values. See `values_ord_metric`.
#' @param range A two-element vector holding the _defaults_ for the smallest and
#'   largest possible values, respectively. If a transformation is specified,
#'   these values should be in the _transformed units_.
#' @param trans A `trans` object from the `scales` package, such as
#'   `scales::transform_log10()` or `scales::transform_reciprocal()`. If not
#'   provided, the default is used which matches the units used in `range`. If
#'   no transformation, `NULL`.
#'
#' @details These functions generate parameters for [parsnip::rand_forest()]
#'   models using the `"ordinalForest"` engine. See `?ordinalForest::ordfor()`
#'   for more details on the original parameters. These parameters are
#'   engine-specific, not general to decision tree models, so are provided here
#'   rather than in `dials`.
#'
#' * `naive_scores()`: Whether to construct only a "naive" ordinal forest using
#'   the scores \eqn{1,2,3,\ldots} for the ordinal values; tunes `naive`.
#'
#' * `num_scores()`: The number of score sets tried prior to optimization; tunes
#'   `nsets`.
#'
#' * `num_score_perms()`: The number of permutations of the class width ordering
#'   to try for each score set tried (after the first); tunes `npermtrial`.
#'
#' * `num_score_trees()`: The number of trees in the score set--specific
#'   forests; tunes `ntreeperdiv`.
#'
#' * `num_scores_best()`: The number of top-performing score sets used to
#'   calculate the optimized score set; tunes `nbest`.
#'
#' * `ord_metric()`: The performance function used to evaluate score
#'   set--specific forests; tunes `perffunction`. See also
#'   `?ordinalForest::perff`.
#'
#' @seealso [dials::trees()]
#' @examples
#' naive_scores()
#' num_scores()
#' num_score_perms()
#' num_score_trees()
#' num_scores_best()
#' ord_metric()

#' @export
#' @rdname ordinalForest_parameters
naive_scores <- function(values = c(FALSE, TRUE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(naive_scores = "Use Naive Ordinal Scores?"),
    finalize = NULL
  )
}

#' @export
#' @rdname ordinalForest_parameters
num_scores <- function(range = c(100L, 2000L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_scores = "# Score Sets Tried"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
num_score_perms <- function(range = c(100L, 500L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_score_perms = "# Class Width Permutations"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
num_score_trees <- function(range = c(10L, 200L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_score_trees = "# Trees per Score Set"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
num_scores_best <- function(range = c(2L, 20L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_scores_best = "# Top Score Sets"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
ord_metric <- function(values = values_ord_metric) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(ord_metric = "Ordinal Performance Function"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
values_ord_metric <-
  # REVIEW: The option `"custom"` is omitted for simplicity as it depends on
  # another parameter.
  c("equal", "probability", "proportional", "oneclass")
