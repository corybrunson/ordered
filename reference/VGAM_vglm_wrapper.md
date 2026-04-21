# Wrappers for `VGAM`

These wrappers convert the standardized `odds_link` options encoded in
[`dials::values_odds_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
to [`VGAM::vglmff`](https://rdrr.io/pkg/VGAM/man/vglmff-class.html)
objects passed to the `family` argument of
[`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html).

## Usage

``` r
VGAM_vglm_wrapper(
  formula,
  data,
  family = "cumulative_link",
  link = "logistic",
  parallel = TRUE,
  ...
)

VGAM_vgam_wrapper(
  formula,
  data,
  family = "cumulative_link",
  link = "logistic",
  parallel = TRUE,
  ...
)

values_ordinal_link_VGAM
```

## Format

An object of class `character` of length 10.

## Arguments

- formula:

  The formula to pass.

- data:

  The data frame to pass.

- ...:

  Additional arguments to pass.

## Value

An object of S3 parent class `VGAM` and primary classes `vglm` or `vgam`
as returned by [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html)
and [`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html).

## Details

Note that [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
[`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html) treat the rows
of `data` as the units of observation: Compressed `data` with one row
per predictor set will be understood as having a multinomial-valued
outcome; only expanded data with one row per case will be understood as
having a single-valued ordinal outcome. (This divide cannot be bridged
by passing a column of counts to the `weights` argument.) These wrappers
require a single ordinal outcome column and therefore do not accept the
convenient `cbind(y1, y2, ...) ~ x1 + x2 + ...` encoding commonly used
in `VGAM`.

## Examples

``` r
values_ordinal_link_VGAM
#>  [1] "logistic" "probit"   "loglog"   "cloglog"  "cauchit"  "foldsqrt"
#>  [7] "logc"     "gord"     "pord"     "nbord"   
dials::ordinal_link(values = values_ordinal_link_VGAM)
#> Ordinal Link (qualitative)
#> 10 possible values include:
#> 'logistic', 'probit', 'loglog', 'cloglog', 'cauchit', 'foldsqrt', 'logc',
#> 'gord', 'pord', and 'nbord'
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
# fit wrapper for linear model
( fit_orig <- VGAM::vglm(
  Sat ~ Type + Infl + Cont,
  family = VGAM::sratio(link = "probitlink", parallel = TRUE),
  data = house_data
) )
#> 
#> Call:
#> VGAM::vglm(formula = Sat ~ Type + Infl + Cont, family = VGAM::sratio(link = "probitlink", 
#>     parallel = TRUE), data = house_data)
#> 
#> 
#> Coefficients:
#> (Intercept):1 (Intercept):2 TypeApartment    TypeAtrium   TypeTerrace 
#>   -0.32396038   -0.08231777    0.29782985    0.21180232    0.58344529 
#>    InflMedium      InflHigh      ContHigh 
#>   -0.30063564   -0.68628214   -0.17688128 
#> 
#> Degrees of Freedom: 3362 Total; 3354 Residual
#> Residual deviance: 3483.463 
#> Log-likelihood: -1741.731 
( fit_wrap <- VGAM_vglm_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "stopping_ratio", link = "probit",
  data = house_data
) )
#> 
#> Call:
#> VGAM::vglm(formula = formula, family = VGAM::sratio(link = "probitlink", 
#>     parallel = TRUE), data = data)
#> 
#> 
#> Coefficients:
#> (Intercept):1 (Intercept):2 TypeApartment    TypeAtrium   TypeTerrace 
#>   -0.32396038   -0.08231777    0.29782985    0.21180232    0.58344529 
#>    InflMedium      InflHigh      ContHigh 
#>   -0.30063564   -0.68628214   -0.17688128 
#> 
#> Degrees of Freedom: 3362 Total; 3354 Residual
#> Residual deviance: 3483.463 
#> Log-likelihood: -1741.731 
# fit wrapper for additive model
( fit_orig <- VGAM::vgam(
  Sat ~ Type + Infl + Cont,
  family = VGAM::cratio(link = "clogloglink", parallel = TRUE),
  data = house_data
) )
#> 
#> Call:
#> VGAM::vgam(formula = Sat ~ Type + Infl + Cont, family = VGAM::cratio(link = "clogloglink", 
#>     parallel = TRUE), data = house_data)
#> 
#> 
#> Degrees of Freedom: 3362 Total; 3354 Residual
#> Residual deviance: 3484.088 
#> Log-likelihood: -1742.044 
( fit_wrap <- VGAM_vgam_wrapper(
  Sat ~ Type + Infl + Cont,
  family = "continuation_ratio", link = "cloglog",
  data = house_data
) )
#> 
#> Call:
#> VGAM::vgam(formula = formula, family = VGAM::cratio(link = "clogloglink", 
#>     parallel = TRUE), data = data)
#> 
#> 
#> Degrees of Freedom: 3362 Total; 3354 Residual
#> Residual deviance: 3484.088 
#> Log-likelihood: -1742.044 
```
