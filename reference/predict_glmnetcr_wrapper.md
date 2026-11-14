# Predict wrappers for `glmnetcr`

Select predictions at a specific penalty value or a vector of penalty
values. For penalties between path values, linearly interpolate between
the probability matrices at the neighboring steps. For penalties outside
the path range, use the path endpoints.

## Usage

``` r
predict_glmnetcr_wrapper(
  object,
  newx,
  type,
  lambda,
  criteria = c("bic", "aic")
)

multi_predict_glmnetcr_wrapper(object, newx, type, lambda)

# S3 method for class '`_glmnetcr`'
predict(
  object,
  new_data,
  type = NULL,
  opts = list(),
  penalty = NULL,
  multi = FALSE,
  ...
)

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

  A penalty value or vector at which to predict. If `NULL`, the step
  minimizing `criteria` is used (single penalty only).

- criteria:

  The criterion by which to select `lambda` within the path sequence.
  Defaults to `"bic"` for consistency with
  [`glmnetcr::predict.glmnetcr()`](https://rdrr.io/pkg/glmnetcr/man/predict.glmnetcr.html).
  (NB: This contrasts with
  [`predict_ordinalNet_wrapper()`](https://corybrunson.github.io/ordered/reference/ordinalNet_wrapper.md).)

- penalty:

  A numeric vector of penalty values. Overrides the default penalty. If
  `NULL`, the regularization path stored in the model fit is used.

## Value

`predict_glmnetcr_wrapper()` returns a character vector of class
predictions or a matrix of class probabilities.
`multi_predict_glmnetcr_wrapper()` returns a numeric array with one
slice per penalty value for class probabilities, or a character matrix
with one column per penalty value for class predictions. Both outputs
mimic those of
[`glmnetcr::predict.glmnetcr()`](https://rdrr.io/pkg/glmnetcr/man/predict.glmnetcr.html).

## Examples

``` r
house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_matrix <- model.matrix(
  Sat ~ Type + Infl + Cont + 0,
  data = house_data,
  contrasts.arg = lapply(house_data[, 2:4], contrasts, contrasts = FALSE)
)
pen_vec <- 10 ^ seq(-1, -2, -.25)
fit <- glmnetcr::glmnetcr(
  house_matrix, y = house_data$Sat,
  lambda = pen_vec
)
#> Warning: Passing 'thresh' to glmnet() is deprecated. Use control = list(thresh = ...) instead.
#> Warning: Passing 'maxit' to glmnet() is deprecated. Use control = list(maxit = ...) instead.
#> Warning: Passing 'dfmax' to glmnet() is deprecated. Use control = list(dfmax = ...) instead.
#> Warning: Passing 'pmax' to glmnet() is deprecated. Use control = list(pmax = ...) instead.
#> Warning: Passing 'trace.it' to glmnet() is deprecated. Use control = list(trace.it = ...) instead.
# predictions (may disagree if `lambda` is not on the path `fit$lambda`)
set.seed(34091)
house_submatrix <- house_matrix[sample(nrow(house_matrix), 6), ]
# single, probability
predict(
  fit,
  newx = house_submatrix
)$probs[, , which.min(abs(fit$lambda - .02))]
#>            Low    Medium      High
#> [1,] 0.3366422 0.2754545 0.3879033
#> [2,] 0.4071144 0.2610529 0.3318327
#> [3,] 0.2558453 0.2828634 0.4612913
#> [4,] 0.4071144 0.2610529 0.3318327
#> [5,] 0.2082957 0.2808630 0.5108413
#> [6,] 0.3121271 0.2788845 0.4089884
predict_glmnetcr_wrapper(
  fit,
  newx = house_submatrix,
  type = "prob",
  lambda = .02
)
#>            Low    Medium      High
#> [1,] 0.3346839 0.2750789 0.3902372
#> [2,] 0.4044382 0.2610532 0.3345086
#> [3,] 0.2640999 0.2816406 0.4542595
#> [4,] 0.4044382 0.2610532 0.3345086
#> [5,] 0.2148620 0.2805043 0.5046337
#> [6,] 0.3140962 0.2779594 0.4079444
# single, class
predict(
  fit,
  newx = house_submatrix
)$class[, which.min(abs(fit$lambda - .02))]
#> [1] "High" "Low"  "High" "Low"  "High" "High"
predict_glmnetcr_wrapper(
  fit,
  newx = house_submatrix,
  type = "class",
  lambda = .02
)
#> [1] "High" "Low"  "High" "Low"  "High" "High"
# multiple, probabilities
predict(
  fit,
  newx = house_submatrix
)$probs
#> , , s0
#> 
#>            Low   Medium      High
#> [1,] 0.3372999 0.265316 0.3973841
#> [2,] 0.3372999 0.265316 0.3973841
#> [3,] 0.3372999 0.265316 0.3973841
#> [4,] 0.3372999 0.265316 0.3973841
#> [5,] 0.3372999 0.265316 0.3973841
#> [6,] 0.3372999 0.265316 0.3973841
#> 
#> , , s1
#> 
#>            Low    Medium      High
#> [1,] 0.3350792 0.2668287 0.3980921
#> [2,] 0.3505946 0.2643341 0.3850713
#> [3,] 0.3350792 0.2668287 0.3980921
#> [4,] 0.3505946 0.2643341 0.3850713
#> [5,] 0.3146669 0.2695901 0.4157430
#> [6,] 0.3350792 0.2668287 0.3980921
#> 
#> , , s2
#> 
#>            Low    Medium      High
#> [1,] 0.3244186 0.2731098 0.4024716
#> [2,] 0.3904093 0.2610549 0.3485358
#> [3,] 0.3073711 0.2752307 0.4173982
#> [4,] 0.3904093 0.2610549 0.3485358
#> [5,] 0.2492828 0.2786238 0.4720934
#> [6,] 0.3244186 0.2731098 0.4024716
#> 
#> , , s3
#> 
#>            Low    Medium      High
#> [1,] 0.3366422 0.2754545 0.3879033
#> [2,] 0.4071144 0.2610529 0.3318327
#> [3,] 0.2558453 0.2828634 0.4612913
#> [4,] 0.4071144 0.2610529 0.3318327
#> [5,] 0.2082957 0.2808630 0.5108413
#> [6,] 0.3121271 0.2788845 0.4089884
#> 
#> , , s4
#> 
#>            Low    Medium      High
#> [1,] 0.3579498 0.2745176 0.3675326
#> [2,] 0.4151922 0.2616352 0.3231726
#> [3,] 0.2204409 0.2848423 0.4947169
#> [4,] 0.4151922 0.2616352 0.3231726
#> [5,] 0.1833356 0.2801171 0.5365473
#> [6,] 0.2858834 0.2843518 0.4297647
#> 
multi_predict_glmnetcr_wrapper(
  fit,
  newx = house_submatrix,
  type = "prob",
  lambda = c(.2, .02, .002)
)
#> , , 1
#> 
#>            Low   Medium      High
#> [1,] 0.3372999 0.265316 0.3973841
#> [2,] 0.3372999 0.265316 0.3973841
#> [3,] 0.3372999 0.265316 0.3973841
#> [4,] 0.3372999 0.265316 0.3973841
#> [5,] 0.3372999 0.265316 0.3973841
#> [6,] 0.3372999 0.265316 0.3973841
#> 
#> , , 2
#> 
#>            Low    Medium      High
#> [1,] 0.3346839 0.2750789 0.3902372
#> [2,] 0.4044382 0.2610532 0.3345086
#> [3,] 0.2640999 0.2816406 0.4542595
#> [4,] 0.4044382 0.2610532 0.3345086
#> [5,] 0.2148620 0.2805043 0.5046337
#> [6,] 0.3140962 0.2779594 0.4079444
#> 
#> , , 3
#> 
#>            Low    Medium      High
#> [1,] 0.3579498 0.2745176 0.3675326
#> [2,] 0.4151922 0.2616352 0.3231726
#> [3,] 0.2204409 0.2848423 0.4947169
#> [4,] 0.4151922 0.2616352 0.3231726
#> [5,] 0.1833356 0.2801171 0.5365473
#> [6,] 0.2858834 0.2843518 0.4297647
#> 
# multiple, class
predict(
  fit,
  newx = house_submatrix
)$class
#>      [,1]   [,2]   [,3]   [,4]   [,5]  
#> [1,] "High" "High" "High" "High" "High"
#> [2,] "High" "High" "Low"  "Low"  "Low" 
#> [3,] "High" "High" "High" "High" "High"
#> [4,] "High" "High" "Low"  "Low"  "Low" 
#> [5,] "High" "High" "High" "High" "High"
#> [6,] "High" "High" "High" "High" "High"
multi_predict_glmnetcr_wrapper(
  fit,
  newx = house_submatrix,
  type = "class",
  lambda = c(.2, .02, .002)
)
#>      0.2    0.02   0.002 
#> [1,] "High" "High" "High"
#> [2,] "High" "Low"  "Low" 
#> [3,] "High" "High" "High"
#> [4,] "High" "Low"  "Low" 
#> [5,] "High" "High" "High"
#> [6,] "High" "High" "High"
```
