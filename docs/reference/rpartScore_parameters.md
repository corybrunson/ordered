# Dials for `rpartScore` engine parameters

These parameters are auxiliary to decision tree models that use the
"rpartScore" engine. They correspond to tuning parameters that would be
specified using `set_engine("rpartScore", ...)`.

## Usage

``` r
split_func(values = c("abs", "quad"))

prune_func(values = c("mr", "mc"))
```

## Arguments

- values:

  A character string of possible values.

## Value

An object of S3 parent class `param` and primary class `qual_param`; see
[`dials::new_qual_param()`](https://dials.tidymodels.org/reference/new-param.html).

## Details

`split_func` and `prune_func` are dials for `split` and `prune`,
respectively. See
[`?rpartScore::rpartScore`](https://rdrr.io/pkg/rpartScore/man/rpartScore.html)
for more details on the original parameters. These parameters are
engine-specific, not general to decision tree models, so are provided
here rather than in `dials`.

## See also

[`dials::trees()`](https://dials.tidymodels.org/reference/trees.html)

## Examples

``` r
split_func()
#> Splitting Function (qualitative)
#> 2 possible values include:
#> 'abs' and 'quad'
prune_func()
#> Pruning Function (qualitative)
#> 2 possible values include:
#> 'mr' and 'mc'
```
