# Dials for `ordinalNet` engine parameters

These parameters are auxiliary to ordinal regression models that use the
"ordinalNet" engine. They correspond to tuning parameters that would be
specified using `set_engine("ordinalNet", ...)`.

## Usage

``` r
parallel_penalty_factor(
  range = c(-Inf, Inf),
  trans = scales::transform_log10()
)
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

## Value

An object of S3 parent class `param` and primary class `quant_param`;
see
[`dials::new_quant_param()`](https://dials.tidymodels.org/reference/new-param.html).

## Details

These functions generate parameters for
[`parsnip::ordinal_reg()`](https://parsnip.tidymodels.org/reference/ordinal_reg.html)
models using the `"ordinalNet"` engine. See `?ordinalNet::ordinalNet()`
for more details on the original parameters. These parameters are
engine-specific, not general to ordinal regression models, so are
provided here rather than in `dials`.

- `parallel_penalty_factor()`: A nonnegative numeric factor that scales
  the penalty on all parallel terms; tunes `parallelPenaltyFactor`. Only
  used when `parallelTerms = TRUE`.

## See also

[`dials::penalty()`](https://dials.tidymodels.org/reference/penalty.html)

## Examples

``` r
parallel_penalty_factor()
#> Parallel Penalty Factor (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): (-Inf, Inf)
```
