# Dials for `ordinal` engine parameters

The `threshold_structure` dial is auxiliary to ordinal regression models
that use the "clm" engine. It corresponds to the `threshold` tuning
parameter that would be specified using `set_engine("clm", ...)`.

## Usage

``` r
threshold_structure(
  values = c("flexible", "symmetric", "symmetric2", "equidistant")
)

values_ordinal_link_clm
```

## Arguments

- values:

  A character string of possible values.

## Value

An object of S3 parent class `param` and primary class `qual_param`; see
[`dials::new_qual_param()`](https://dials.tidymodels.org/reference/new-param.html).

## Details

The vector `values_ordinal_link_clm` extends the default `ordinal_link`
options encoded in
[`dials::values_ordinal_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
to those accepted by
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html).

`threshold_structure()` is a dial for the threshold structure in
cumulative link models. See
[`?ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) for more
details. Use `set_args(threshold = ...)` to set this parameter on a
model spec. These parameters are engine-specific, not general to ordinal
regression models, so are provided here rather than in `dials`.

## Examples

``` r
values_ordinal_link_clm
#> [1] "logistic"     "probit"       "loglog"       "cloglog"      "cauchit"     
#> [6] "Aranda-Ordaz" "log-gamma"   
dials::ordinal_link(values = values_ordinal_link_clm)
#> Ordinal Link (qualitative)
#> 7 possible values include:
#> 'logistic', 'probit', 'loglog', 'cloglog', 'cauchit', 'Aranda-Ordaz', and
#> 'log-gamma'
threshold_structure()
#> Threshold Structure (qualitative)
#> 4 possible values include:
#> 'flexible', 'symmetric', 'symmetric2', and 'equidistant'
```
