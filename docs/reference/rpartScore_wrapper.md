# A wrapper for `rpartScore`

A wrapper is used because the model interface requires the response
variable to be numeric rather than ordered or factor; the wrapper edits
the input `data` accordingly.

## Usage

``` r
rpartScore_wrapper(formula, data, ...)
```

## Arguments

- formula:

  The formula to pass.

- data:

  The data frame to pass.

- ...:

  Additional arguments to pass.

## Value

An object of S3 class `rpart` as returned by
[`rpartScore::rpartScore()`](https://rdrr.io/pkg/rpartScore/man/rpartScore.html).

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
# subsample to reduce runtime
house_data <- house_data[sample(nrow(house_data), nrow(house_data) / 5), ]
# fit wrapper
( fit_orig <- rpartScore::rpartScore(
  formula = Sat ~ Infl + Type + Cont,
  data = transform(house_data, Sat = as.integer(Sat))
) )
#> n= 336 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 336 247 2  
#>    2) Infl=Low,Medium 262 187 2  
#>      4) Type=Apartment,Terrace 172 124 2  
#>        8) Infl=Medium 88  60 2 *
#>        9) Infl=Low 84  56 1  
#>         18) Type=Apartment 60  43 2 *
#>         19) Type=Terrace 24   9 1 *
#>      5) Type=Tower,Atrium 90  63 2 *
#>    3) Infl=High 74  38 3 *
( fit_wrap <- rpartScore_wrapper(
  formula = Sat ~ Infl + Type + Cont,
  data = house_data
) )
#> n= 336 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 336 247 2  
#>    2) Infl=Low,Medium 262 187 2  
#>      4) Type=Apartment,Terrace 172 124 2  
#>        8) Infl=Medium 88  60 2 *
#>        9) Infl=Low 84  56 1  
#>         18) Type=Apartment 60  43 2 *
#>         19) Type=Terrace 24   9 1 *
#>      5) Type=Tower,Atrium 90  63 2 *
#>    3) Infl=High 74  38 3 *
```
