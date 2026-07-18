# A wrapper for `orf`

The `orf` package uses a non-standard interface that requires the
predictor matrix `X` and the outcome vector `Y` as separate arguments,
with `Y` as a numeric vector. This wrapper converts the parsnip-standard
data.frame (`X`) and factor (`Y`) to the required formats before calling
[`orf::orf()`](https://rdrr.io/pkg/orf/man/orf.html).

## Usage

``` r
orf_wrapper(X, Y, ...)
```

## Arguments

- X:

  The predictor data (data.frame).

- Y:

  The outcome factor.

- ...:

  Arguments to pass to
  [`orf::orf()`](https://rdrr.io/pkg/orf/man/orf.html).

## Value

An object of S3 class `orf` as returned by
[`orf::orf()`](https://rdrr.io/pkg/orf/man/orf.html).

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_data <- house_data[sample(nrow(house_data), 50), ]
# encode factors as dummy variables before passing to wrapper
X_mat <- model.matrix(~ Infl + Type + Cont - 1, data = house_data)
# fit wrapper
(fit_wrap <- orf_wrapper(
  X = X_mat,
  Y = house_data$Sat,
  num.trees = 10
))
#> Ordered Forest object of class orf 
#> 
#> Number of Categories:             3 
#> Sample Size:                      50 
#> Number of Trees:                  10 
#> Build:                            Subsampling 
#> Mtry:                             3 
#> Minimum Node Size:                5 
#> Honest Forest:                    TRUE 
#> Weight-Based Inference:           FALSE 
```
