# Wrapper for `rms` predictions

[`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) and
[`rms::orm()`](https://rdrr.io/pkg/rms/man/orm.html) use
[`rms::predictrms()`](https://rdrr.io/pkg/rms/man/predictrms.html)
internally, which does not accept tibble inputs. This wrapper ensures
that `newdata` is converted to a plain data frame before the S3 method
is dispatched.

## Usage

``` r
predict_lrm_wrapper(object, newdata, type, ...)
```

## Arguments

- object:

  A fitted model object of class `"lrm"` or `"orm"`.

- newdata:

  A data frame or tibble of predictors.

- type:

  The prediction type, passed to the underlying method.

- ...:

  Additional arguments passed to the underlying method.

## Value

The result of [`stats::predict()`](https://rdrr.io/r/stats/predict.html)
dispatched on `object`.

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
( fit <- rms::lrm(Sat ~ Infl + Type + Cont, data = house_data) )
#> Logistic Regression Model
#> 
#> rms::lrm(formula = Sat ~ Infl + Type + Cont, data = house_data)
#> 
#>                         Model Likelihood       Discrimination    Rank Discrim.    
#>                               Ratio Test              Indexes          Indexes    
#> Obs           1681    LR chi2     169.73       R2       0.108    C       0.650    
#>  Low           567    d.f.             6      R2(6,1681)0.093    Dxy     0.299    
#>  Medium        446    Pr(> chi2) <0.0001    R2(6,1479.6)0.105    gamma   0.316    
#>  High          668                             Brier    0.207    tau-a   0.197    
#> max |deriv| 0.0001                                                                
#> 
#>                Coef    S.E.   Wald Z Pr(>|Z|)
#> y>=Medium       0.4961 0.1248  3.97  <0.0001 
#> y>=High        -0.6907 0.1255 -5.50  <0.0001 
#> Infl=Medium     0.5664 0.1047  5.41  <0.0001 
#> Infl=High       1.2888 0.1272 10.14  <0.0001 
#> Type=Apartment -0.5724 0.1192 -4.80  <0.0001 
#> Type=Atrium    -0.3662 0.1552 -2.36  0.0183  
#> Type=Terrace   -1.0910 0.1515 -7.20  <0.0001 
#> Cont=High       0.3603 0.0955  3.77  0.0002  
#> 
# predict wrapper
predict(
  fit,
  newdata = head(house_data, 3),
  type = "fitted.ind"
)
#>       Sat=Low Sat=Medium  Sat=High
#> 1   0.3784494  0.2876751 0.3338755
#> 1.1 0.3784494  0.2876751 0.3338755
#> 1.2 0.3784494  0.2876751 0.3338755
# tibble input is converted internally
predict_lrm_wrapper(
  fit,
  newdata = tibble::as_tibble(head(house_data, 3)),
  type = "fitted.ind"
)
#>     Sat=Low Sat=Medium  Sat=High
#> 1 0.3784494  0.2876751 0.3338755
#> 2 0.3784494  0.2876751 0.3338755
#> 3 0.3784494  0.2876751 0.3338755
```
