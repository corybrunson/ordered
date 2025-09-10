#' Dials for ordinalForest engine parameters
#'
#' These parameters are auxiliary to random forest models that use the
#' "ordinalForest" engine. They correspond to tuning parameters that would be
#' specified using `set_engine("ordinalForest", ...)`.
#'
#' @name ordinalForest_parameters
#' @param values A character string of possible values. These functions generate
#'   parameters for [parsnip::rand_forest()] models using the `"ordinalForest"`
#'   engine.
#'
#' * `naive_scores()`: Whether to construct only a "naive" ordinal forest using
#'   the scores \eqn{1,2,3,\ldots} for the ordinal values. (See the `naive`
#'   argument at `?ordinalForest::ordfor()`.)
#'
#' * `num_scores()`: The number of score sets tried prior to optimization. (See
#'   the `nsets` argument at `?ordinalForest::ordfor()`.)
#'
#' * `num_scores_perm()`: The number of permutations of the class width ordering
#'   to try for each score set tried (after the first). (See the `npermtrial`
#'   argument at `?ordinalForest::ordfor()`.)
#'
#' * `num_scores_trees()`: The number of trees in the score set--specific
#'   forests. (See the `ntreeperdiv` argument at `?ordinalForest::ordfor()`.)
#'
#' * `num_scores_best()`: The number of top-performing score sets used to
#'   calculate the optimized score set. (See the `nbest` argument at
#'   `?ordinalForest::ordfor()`.)
#'
#' * `ord_metric()`: The performance function used to evaluate score
#'   set--specific forests. (See the `perffunction` argument at
#'   `?ordinalForest::ordfor()` and also `?ordinalForest::perff`.)
#'
#'   These parameters are engine-specific, not general to decision tree models,
#'   so are provided here rather than in `dials`.
#' @seealso [dials::trees()]
#' @examples
#' naive_scores()
#' num_scores()
#' num_scores_perm()
#' num_scores_trees()
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
num_scores_perm <- function(range = c(100L, 500L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_scores_perm = "# Class Width Permutations"),
    finalize = NULL
  )
}

#' @rdname ordinalForest_parameters
#' @export
num_scores_trees <- function(range = c(10L, 200L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_scores_trees = "# Trees per Score Set"),
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
