# Prediction wrapper for `glmnetcr`

Selects predictions at a specific penalty value from the regularization
path. When the requested penalty lies between two path values, linearly
interpolates between the probability matrices at the neighboring steps.

## Usage

``` r
predict_glmnetcr_wrapper(
  object,
  newx,
  type,
  lambda,
  criteria = c("bic", "aic")
)

# S3 method for class '`_glmnetcr`'
predict(object, new_data, type = NULL, opts = list(), penalty = NULL, ...)

# S3 method for class '`_glmnetcr`'
predict_class(object, new_data, ...)

# S3 method for class '`_glmnetcr`'
predict_classprob(object, new_data, ...)

# S3 method for class '`_glmnetcr`'
multi_predict(
  object,
  new_data,
  type = NULL,
  opts = list(),
  penalty = NULL,
  ...
)
```

## Arguments

- object:

  A `glmnetcr` object.

- newx:

  A predictor matrix.

- type:

  Either `"class"` or `"prob"`.

- lambda:

  A penalty value at which to predict. If `NULL`, the step minimizing
  `criteria` is used.

- criteria:

  Criterion by which to select `lambda` within the path sequence.
  Defaults to `"bic"` for consistency with
  [`glmnetcr::predict.glmnetcr()`](https://rdrr.io/pkg/glmnetcr/man/predict.glmnetcr.html).
  (NB: This contrasts with
  [`predict_ordinalNet_wrapper()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md).)

- penalty:

  A numeric vector of penalty values. Overrides the default penalty. If
  `NULL`, the regularization path stored in the model fit is used.

## Value

A character vector of class predictions or a matrix of class
probabilities.

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_matrix <- model.matrix(
  Sat ~ Type + Infl + Cont + 0,
  data = house_data,
  contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
)
pen_vec <- 10 ^ seq(-1, -3, -.5)
fit <- glmnetcr::glmnetcr(
  house_matrix, y = house_data$Sat,
  lambda = pen_vec
)
#> Warning: Passing 'thresh' to glmnet() is deprecated. Use control = list(thresh = ...) instead.
#> Warning: Passing 'maxit' to glmnet() is deprecated. Use control = list(maxit = ...) instead.
#> Warning: Passing 'dfmax' to glmnet() is deprecated. Use control = list(dfmax = ...) instead.
#> Warning: Passing 'pmax' to glmnet() is deprecated. Use control = list(pmax = ...) instead.
#> Warning: Passing 'trace.it' to glmnet() is deprecated. Use control = list(trace.it = ...) instead.
#> Warning: from glmnet C++ code (error code -10004); Number of nonzero coefficients along the path exceeds pmax=9 at 4th lambda value; solutions for larger lambdas returned
# predictions (may disagree if `lambda` is not on the path `fit$lambda`)
# probability
predict(
  fit,
  newx = head(house_matrix)
)$probs[, , which.min(abs(fit$lambda - 0.02))]
#>            Low    Medium     High
#> [1,] 0.3784433 0.2703317 0.351225
#> [2,] 0.3784433 0.2703317 0.351225
#> [3,] 0.3784433 0.2703317 0.351225
#> [4,] 0.3784433 0.2703317 0.351225
#> [5,] 0.3784433 0.2703317 0.351225
#> [6,] 0.3784433 0.2703317 0.351225
predict_glmnetcr_wrapper(
  fit,
  newx = head(house_matrix),
  type = "prob",
  lambda = 0.02
)
#>            Low    Medium     High
#> [1,] 0.3755644 0.2678286 0.356607
#> [2,] 0.3755644 0.2678286 0.356607
#> [3,] 0.3755644 0.2678286 0.356607
#> [4,] 0.3755644 0.2678286 0.356607
#> [5,] 0.3755644 0.2678286 0.356607
#> [6,] 0.3755644 0.2678286 0.356607
# class
predict(
  fit,
  newx = head(house_matrix)
)$class[, which.min(abs(fit$lambda - 0.02))]
#> [1] "Low" "Low" "Low" "Low" "Low" "Low"
predict_glmnetcr_wrapper(
  fit,
  newx = head(house_matrix),
  type = "class",
  lambda = 0.02
)
#> [1] "Low" "Low" "Low" "Low" "Low" "Low"
```
