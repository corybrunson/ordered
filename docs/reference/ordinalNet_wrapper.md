# Wrappers for `ordinalNet`

The fit wrapper converts the standardized `odds_link` options encoded in
[`dials::values_odds_link`](https://dials.tidymodels.org/reference/ordinal_link.html)
to the `family` options of
[`ordinalNet::ordinalNet()`](https://rdrr.io/pkg/ordinalNet/man/ordinalNet.html).
The prediction wrapper interpolates between fitted penalties to enable
submodel prediction at specified penalties.

## Usage

``` r
ordinalNet_wrapper(
  x,
  y,
  weights = NULL,
  family = "cumulative_link",
  link = "logistic",
  ...
)

predict_ordinalNet_wrapper(
  object,
  newx,
  type,
  lambda,
  criteria = c("aic", "bic")
)

# S3 method for class '`_ordinalNet`'
predict(
  object,
  new_data,
  type = NULL,
  opts = list(),
  penalty = NULL,
  multi = FALSE,
  ...
)

# S3 method for class '`_ordinalNet`'
multi_predict(
  object,
  new_data,
  type = NULL,
  opts = list(),
  penalty = NULL,
  ...
)

# S3 method for class '`_ordinalNet`'
predict_raw(object, new_data, opts = list(), ...)

# S3 method for class '`_ordinalNet`'
predict_classprob(object, new_data, ...)

# S3 method for class '`_ordinalNet`'
predict_class(object, new_data, ...)
```

## Arguments

- x:

  The predictor data.

- y:

  The outcome vector.

- ...:

  Additional arguments to pass.

- penalty:

  A numeric vector of penalty values.

## Value

An object of S3 class `ordinalNet` as returned by
[`ordinalNet::ordinalNet()`](https://rdrr.io/pkg/ordinalNet/man/ordinalNet.html),
or a vector or matrix of predictions as returned by
[`ordinalNet::predict.ordinalNet`](https://rdrr.io/pkg/ordinalNet/man/predict.ordinalNet.html)`()`.

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_matrix <- model.matrix(
  Sat ~ Type + Infl + Cont + 0,
  data = house_data,
  contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
)
pen_vec <- 10 ^ seq(-2.5, -.5, 1)
# fit wrapper
( fit_orig <- ordinalNet::ordinalNet(
  house_matrix, y = house_data$Sat,
  family = "sratio", link = "logit",
  lambdaVals = pen_vec
) )
#> 
#> Summary of fit:
#> 
#>    lambdaVals nNonzero    loglik     devPct      aic      bic
#> 1 0.316227766        2 -1824.439 0.00000000 3652.878 3663.732
#> 2 0.031622777        8 -1753.373 0.03895240 3522.745 3566.162
#> 3 0.003162278        8 -1741.769 0.04531243 3499.538 3542.955
#> 
( fit_wrap <- ordinalNet_wrapper(
  house_matrix, y = house_data$Sat,
  family = "stopping_ratio", link = "logistic",
  lambdaVals = pen_vec
) )
#> 
#> Summary of fit:
#> 
#>    lambdaVals nNonzero    loglik     devPct      aic      bic
#> 1 0.316227766        2 -1824.439 0.00000000 3652.878 3663.732
#> 2 0.031622777        8 -1753.373 0.03895240 3522.745 3566.162
#> 3 0.003162278        8 -1741.769 0.04531243 3499.538 3542.955
#> 
fit_tidy <-
  ordinal_reg(ordinal_link = "logistic", odds_link = "stopping_ratio") %>%
  set_engine("ordinalNet") %>%
  set_args(path_values = pen_vec, penalty = 1) %>%
  fit(formula = Sat ~ Type + Infl + Cont + 0, data = house_data)
fit_tidy$fit
#> 
#> Summary of fit:
#> 
#>    lambdaVals nNonzero    loglik     devPct      aic      bic
#> 1 0.316227766        2 -1824.439 0.00000000 3652.878 3663.732
#> 2 0.031622777        8 -1753.373 0.03895240 3522.745 3566.162
#> 3 0.003162278        8 -1741.769 0.04531243 3499.538 3542.955
#> 
# predict wrapper
predict(
  fit_orig,
  newx = head(house_matrix),
  whichLambda = 2,
  type = "response"
)
#>         P[Y=1]    P[Y=2]    P[Y=3]
#> [1,] 0.3602237 0.2833106 0.3564657
#> [2,] 0.3602237 0.2833106 0.3564657
#> [3,] 0.3602237 0.2833106 0.3564657
#> [4,] 0.3602237 0.2833106 0.3564657
#> [5,] 0.3602237 0.2833106 0.3564657
#> [6,] 0.3602237 0.2833106 0.3564657
predict_ordinalNet_wrapper(
  fit_tidy$fit,
  newx = head(house_matrix),
  type = "prob",
  lambda = pen_vec[2]
)
#>         P[Y=1]    P[Y=2]    P[Y=3]
#> [1,] 0.3602237 0.2833106 0.3564657
#> [2,] 0.3602237 0.2833106 0.3564657
#> [3,] 0.3602237 0.2833106 0.3564657
#> [4,] 0.3602237 0.2833106 0.3564657
#> [5,] 0.3602237 0.2833106 0.3564657
#> [6,] 0.3602237 0.2833106 0.3564657
predict_ordinalNet_wrapper(
  fit_tidy$fit,
  newx = head(house_matrix),
  type = "prob",
  lambda = .01
)
#>         P[Y=1]    P[Y=2]    P[Y=3]
#> [1,] 0.3670833 0.2900539 0.3428629
#> [2,] 0.3670833 0.2900539 0.3428629
#> [3,] 0.3670833 0.2900539 0.3428629
#> [4,] 0.3670833 0.2900539 0.3428629
#> [5,] 0.3670833 0.2900539 0.3428629
#> [6,] 0.3670833 0.2900539 0.3428629
```
