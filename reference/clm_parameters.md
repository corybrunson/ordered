# Dials for `ordinal` engine parameters

The vector `values_ordinal_link_clm` extends the default `ordinal_link`
options encoded in
[`dials::values_ordinal_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
to those accepted by
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html).

## Usage

``` r
values_ordinal_link_clm
```

## Value

An object of S3 parent class `param` and primary class `qual_param`; see
[`dials::new_qual_param()`](https://dials.tidymodels.org/reference/new-param.html).

## Details

The `threshold_structure` dial is defined in the dials package. See
[`?dials::threshold_structure`](https://dials.tidymodels.org/reference/threshold_structure.html)
for details.

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
dials::threshold_structure(values = dials::values_threshold_structure)
#> Threshold Structure (qualitative)
#> 4 possible values include:
#> 'flexible', 'symmetric_median', 'symmetric_zero', and 'equidistant'
```
