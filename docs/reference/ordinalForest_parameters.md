# Dials for `ordinalForest` engine parameters

These parameters are auxiliary to random forest models that use the
"ordinalForest" engine. They correspond to tuning parameters that would
be specified using `set_engine("ordinalForest", ...)`.

## Usage

``` r
naive_scores(values = c(FALSE, TRUE))

num_scores(range = c(100L, 2000L), trans = NULL)

num_score_perms(range = c(100L, 500L), trans = NULL)

num_score_trees(range = c(10L, 200L), trans = NULL)

num_scores_best(range = c(2L, 20L), trans = NULL)

ord_metric(values = values_ord_metric)

values_ord_metric
```

## Format

An object of class `character` of length 4.

## Arguments

- values:

  A character string of possible values. See `values_ord_metric`.

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

An object of S3 parent class `param` and primary class `qual_param` or
`quant_param`; see
[`dials::new_qual_param()`](https://dials.tidymodels.org/reference/new-param.html)
and \[dials::new_quant_param().

## Details

These functions generate parameters for
[`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
models using the `"ordinalForest"` engine. See
`?ordinalForest::ordfor()` for more details on the original parameters.
These parameters are engine-specific, not general to decision tree
models, so are provided here rather than in `dials`.

- `naive_scores()`: Whether to construct only a "naive" ordinal forest
  using the scores \\(1,2,3,\ldots\\) for the ordinal values; tunes
  `naive`.

- `num_scores()`: The number of score sets tried prior to optimization;
  tunes `nsets`.

- `num_score_perms()`: The number of permutations of the class width
  ordering to try for each score set tried (after the first); tunes
  `npermtrial`.

- `num_score_trees()`: The number of trees in the score set–specific
  forests; tunes `ntreeperdiv`.

- `num_scores_best()`: The number of top-performing score sets used to
  calculate the optimized score set; tunes `nbest`.

- `ord_metric()`: The performance function used to evaluate score
  set–specific forests; tunes `perffunction`. See also
  [`?ordinalForest::perff`](https://rdrr.io/pkg/ordinalForest/man/perff.html).

## See also

[`dials::trees()`](https://dials.tidymodels.org/reference/trees.html)

## Examples

``` r
naive_scores()
#> Use Naive Ordinal Scores? (qualitative)
#> 2 possible values include:
#> FALSE and TRUE
num_scores()
#> # Score Sets Tried (quantitative)
#> Range: [100, 2000]
num_score_perms()
#> # Class Width Permutations (quantitative)
#> Range: [100, 500]
num_score_trees()
#> # Trees per Score Set (quantitative)
#> Range: [10, 200]
num_scores_best()
#> # Top Score Sets (quantitative)
#> Range: [2, 20]
ord_metric()
#> Ordinal Performance Function (qualitative)
#> 4 possible values include:
#> 'equal', 'probability', 'proportional', and 'oneclass'
```
