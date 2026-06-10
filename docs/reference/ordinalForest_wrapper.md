# A wrapper for `ordinalForest`

A wrapper is needed since they have a non-standard model interface that
required the data set and the column name (character string) for the
outcome.

## Usage

``` r
ordinalForest_wrapper(x, y, ...)
```

## Arguments

- x:

  The predictor data.

- y:

  The outcome factor.

- ...:

  Arguments to pass to the underlying model function.

## Value

An object of S3 class `ordfor` as returned by
[`ordinalForest::ordfor()`](https://rdrr.io/pkg/ordinalForest/man/ordfor.html).

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
# subsample to reduce runtime
house_data <- house_data[sample(nrow(house_data), nrow(house_data) / 10), ]
# fit wrapper
# (using inadvisedly few score sets and trees to reduce runtime)
( fit_orig <- ordinalForest::ordfor(
  depvar = "Sat",
  data = house_data,
  nsets = 25, ntreefinal = 100
) )
#> 
#> Ordinal forest 
#> 
#> Number of observations: 168, number of covariates: 3 
#> 
#> Classes of ordinal target variable: 
#> "Low" (n = 52), "Medium" (n = 49), "High" (n = 67) 
#> 
#> Forest setup: 
#> Number of trees in ordinal forest: 100 
#> Number of considered score sets in total: 25 
#> Number of best score sets used for approximating the optimal score set: 10 
#> Number of trees per regression forests constructed in the optimization: 100 
#> Performance function: "equal" 
( fit_wrap <- ordinalForest_wrapper(
  x = subset(house_data, select = -Sat),
  y = house_data$Sat,
  nsets = 25, ntreefinal = 100
) )
#> 
#> Ordinal forest 
#> 
#> Number of observations: 168, number of covariates: 3 
#> 
#> Classes of ordinal target variable: 
#> "Low" (n = 52), "Medium" (n = 49), "High" (n = 67) 
#> 
#> Forest setup: 
#> Number of trees in ordinal forest: 100 
#> Number of considered score sets in total: 25 
#> Number of best score sets used for approximating the optimal score set: 10 
#> Number of trees per regression forests constructed in the optimization: 100 
#> Performance function: "equal" 
```
