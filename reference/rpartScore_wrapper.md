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
#>  1) root 336 245 2  
#>    2) Infl=High 79  45 3 *
#>    3) Infl=Low,Medium 257 181 2  
#>      6) Type=Apartment,Atrium 160 109 2  
#>       12) Infl=Low 90  65 2  
#>         24) Type=Apartment 65  45 1 *
#>         25) Type=Atrium 25  15 2 *
#>       13) Infl=Medium 70  44 2 *
#>      7) Type=Tower,Terrace 97  72 2  
#>       14) Type=Tower 56  43 2  
#>         28) Cont=Low 32  23 2 *
#>         29) Cont=High 24  16 3 *
#>       15) Type=Terrace 41  22 1 *
( fit_wrap <- rpartScore_wrapper(
  formula = Sat ~ Infl + Type + Cont,
  data = house_data
) )
#> n= 336 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 336 245 2  
#>    2) Infl=High 79  45 3 *
#>    3) Infl=Low,Medium 257 181 2  
#>      6) Type=Apartment,Atrium 160 109 2  
#>       12) Infl=Low 90  65 2  
#>         24) Type=Apartment 65  45 1 *
#>         25) Type=Atrium 25  15 2 *
#>       13) Infl=Medium 70  44 2 *
#>      7) Type=Tower,Terrace 97  72 2  
#>       14) Type=Tower 56  43 2  
#>         28) Cont=Low 32  23 2 *
#>         29) Cont=High 24  16 3 *
#>       15) Type=Terrace 41  22 1 *
```
