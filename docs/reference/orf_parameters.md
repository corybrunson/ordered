# Dials for `orf` engine parameters

These parameters are auxiliary to random forest models that use the
"orf" engine. They correspond to tuning parameters that would be
specified using `set_engine("orf", ...)`.

## Usage

``` r
sample_fraction(range = c(0.1, 1), trans = NULL)

honesty(values = c(TRUE, FALSE))

honesty_fraction(range = c(0.1, 0.9), trans = NULL)
```

## Arguments

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

- values:

  A logical vector of possible values. See `values_ord_metric`.

## Value

An object of S3 parent class `param` and primary class `qual_param` or
`quant_param`; see
[`dials::new_qual_param()`](https://dials.tidymodels.org/reference/new-param.html)
and
[`dials::new_quant_param()`](https://dials.tidymodels.org/reference/new-param.html).

## Details

These functions generate parameters for
[`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
models using the `"orf"` engine. See `?orf::orf()` for more details on
the original parameters. These parameters are engine-specific, not
general to random forest models, so are provided here rather than in
`dials`.

- `sample_fraction()`: The fraction of observations to be subsampled;
  tunes `sample.fraction`.

- `honesty()`: Whether to use honest splitting (sample splitting); tunes
  `honesty`.

- `honesty_fraction()`: The fraction of observations reserved for the
  honest (estimation) sample; tunes `honesty.fraction`.

## See also

[`dials::trees()`](https://dials.tidymodels.org/reference/trees.html)

## Examples

``` r
sample_fraction()
#> Subsampling Fraction (quantitative)
#> Range: (0.1, 1]
honesty()
#> Honest Splitting? (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
honesty_fraction()
#> Honest Sample Fraction (quantitative)
#> Range: (0.1, 0.9)
```
