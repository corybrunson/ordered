# Fit wrapper for `ordinal::clm()`

Fit `clm()`, translating `parallel_reg` into the `nominal` formula. This
is done here rather than in
[`parsnip::translate()`](https://parsnip.tidymodels.org/reference/translate.html)
because the formula can only be split once `data` is known, and because
the standard translation (`make_form_call()`) overwrites any location
formula that
[`translate()`](https://parsnip.tidymodels.org/reference/translate.html)
sets.

## Usage

``` r
clm_wrapper(
  formula,
  data,
  weights = NULL,
  parallel_reg = NULL,
  link = NULL,
  threshold = NULL,
  ...,
  call = rlang::caller_env()
)
```

## Arguments

- formula:

  The formula to pass.

- data:

  The data frame to pass.

- weights:

  An optional vector of case weights to pass.

- parallel_reg:

  Logical; whether predictor effects are shared across thresholds. When
  `FALSE`, every predictor is moved to a `nominal` formula.

- ...:

  Additional arguments to pass.

- call:

  The execution environment of a currently running function.

## Value

An object of class `clm` as returned by
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html).

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
# arguments are translated
( fit_orig <- ordinal::clm(
  Sat ~ Type + Cont, data = house_data,
  link = "logit", threshold = "symmetric2"
) )
#> formula: Sat ~ Type + Cont
#> data:    house_data
#> 
#>  link  threshold  nobs logLik   AIC     niter max.grad cond.H 
#>  logit symmetric2 1681 -1802.96 3615.92 4(0)  2.27e-08 4.4e+01
#> 
#> Coefficients:
#> TypeApartment    TypeAtrium   TypeTerrace      ContHigh 
#>     -0.157157     -0.001148     -0.776497      0.417898 
#> 
#> Threshold coefficients:
#> spacing.1 
#>    0.5576 
( fit_wrap <- clm_wrapper(
  Sat ~ Type + Cont, data = house_data,
  link = "logistic", threshold = "symmetric_zero"
) )
#> formula: Sat ~ Type + Cont
#> data:    data
#> 
#>  link  threshold  nobs logLik   AIC     niter max.grad cond.H 
#>  logit symmetric2 1681 -1802.96 3615.92 4(0)  2.27e-08 4.4e+01
#> 
#> Coefficients:
#> TypeApartment    TypeAtrium   TypeTerrace      ContHigh 
#>     -0.157157     -0.001148     -0.776497      0.417898 
#> 
#> Threshold coefficients:
#> spacing.1 
#>    0.5576 
# relax the parallel regression assumption
( fit_orig <- ordinal::clm(
  Sat ~ 1, data = house_data,
  nominal = ~ Infl + Cont
) )
#> formula: Sat ~ 1
#> nominal: ~Infl + Cont
#> data:    house_data
#> 
#>  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#>  logit flexible  1681 -1766.19 3548.37 5(0)  4.75e-13 6.9e+01
#> 
#> Threshold coefficients:
#>             Low|Medium Medium|High
#> (Intercept)  0.01016    1.08136   
#> InflMedium  -0.60952   -0.55768   
#> InflHigh    -1.23466   -1.30074   
#> ContHigh    -0.33926   -0.20760   
( fit_wrap <- clm_wrapper(
  Sat ~ Infl + Cont, data = house_data,
  parallel_reg = FALSE
) )
#> formula: Sat ~ 1
#> nominal: ~Infl + Cont
#> data:    data
#> 
#>  link  threshold nobs logLik   AIC     niter max.grad cond.H 
#>  logit flexible  1681 -1766.19 3548.37 5(0)  4.75e-13 6.9e+01
#> 
#> Threshold coefficients:
#>             Low|Medium Medium|High
#> (Intercept)  0.01016    1.08136   
#> InflMedium  -0.60952   -0.55768   
#> InflHigh    -1.23466   -1.30074   
#> ContHigh    -0.33926   -0.20760   
```
