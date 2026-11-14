# Fit and predict wrappers for `ordinalNet`

The fit wrapper restructures case weights and reorganizes arguments into
a call to
[`ordinalNet::ordinalNet()`](https://rdrr.io/pkg/ordinalNet/man/ordinalNet.html).
The prediction wrapper interpolates between fitted penalties to enable
submodel prediction at specified penalties.

## Usage

``` r
ordinalNet_wrapper(
  x,
  y,
  weights = NULL,
  family = "cumulative",
  link = "logit",
  parallel_reg = NULL,
  parallelTerms = TRUE,
  nonparallelTerms = FALSE,
  parallelPenaltyFactor = 1,
  ...,
  call = rlang::caller_env()
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

- weights:

  An optional numeric vector of case weights. When provided, the outcome
  is restructured into a multinomial matrix of weighted indicators.

- family:

  The odds link function; either a standardized dial value
  (`"cumulative_link"`, `"adjacent_categories"`, `"continuation_ratio"`,
  `"stopping_ratio"`) or an `ordinalNet` native value (`"cumulative"`,
  `"acat"`, `"cratio"`, `"sratio"`).

- link:

  The ordinal link function; either a standardized dial value (e.g.
  `"logistic"`, `"probit"`) or a `ordinalNet` native value (e.g.
  `"logit"`, `"probit"`).

- parallel_reg:

  Logical; whether predictors share effects across thresholds. When
  `FALSE`, `parallelTerms` is set to `FALSE` and `nonparallelTerms` to
  `TRUE`.

- parallelTerms:

  Logical; whether to use parallel terms.

- nonparallelTerms:

  Logical; whether to use non-parallel terms.

- parallelPenaltyFactor:

  Numeric; scale factor applied to the penalty on parallel terms. Errs
  when used without parallel terms.

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
