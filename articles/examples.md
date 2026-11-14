# Fitting and Predicting with ordered

These examples illustrate which models, engines, and prediction types
are available in ordered. As a reminder, in parsnip,

- the **model type** differentiates basic modeling approaches, such as
  random forests and ordinal regression;

- the **mode** denotes in what kind of modeling context it will be used
  (here, classification); and

- the computational **engine** indicates how the model is fit, such as
  with a specific R package implementation or even methods outside of R
  like Keras or Stan.

The following examples use the same data set throughout.

## `decision_tree()` models

With the `"rpartScore"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

dt_spec <-
  decision_tree() |>
  set_engine("rpartScore", split = "quad") |>
  set_mode("classification")
dt_spec
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   split = quad
    ## 
    ## Computational engine: rpartScore

Now we create the model fit object:

``` r

set.seed(1)
dt_fit <- dt_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
dt_fit
```

    ## parsnip model object
    ## 
    ## n= 1669 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ## 1) root 1669 1225 2  
    ##   2) Infl=Low 624  454 2  
    ##     4) Type=Apartment,Terrace 391  258 1 *
    ##     5) Type=Tower,Atrium 233  161 2 *
    ##   3) Infl=Medium,High 1045  771 2  
    ##     6) Infl=Medium 656  468 2 *
    ##     7) Infl=High 389  242 3 *

The holdout data can be predicted for the most likely class:

``` r

predict(dt_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 Medium     
    ##  2 High       
    ##  3 High       
    ##  4 High       
    ##  5 High       
    ##  6 High       
    ##  7 Medium     
    ##  8 Low        
    ##  9 Medium     
    ## 10 Medium     
    ## 11 Medium     
    ## 12 High

## `gen_additive_mod()` models

With the `"vgam"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

gam_spec <-
  gen_additive_mod() |>
  set_engine("vgam", family = "stopping_ratio") |>
  set_mode("classification")
gam_spec
```

    ## GAM Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   family = stopping_ratio
    ## 
    ## Computational engine: vgam

Now we create the model fit object:

``` r

set.seed(1)
gam_fit <- gam_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
gam_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## VGAM::vgam(formula = formula, family = VGAM::sratio(link = "logitlink", 
    ##     parallel = FALSE, Thresh = NULL), data = data)
    ## 
    ## 
    ## Degrees of Freedom: 3338 Total; 3324 Residual
    ## Residual deviance: 3447.008 
    ## Log-likelihood: -1723.504

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(gam_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 High       
    ##  3 High       
    ##  4 Low        
    ##  5 High       
    ##  6 High       
    ##  7 Low        
    ##  8 High       
    ##  9 High       
    ## 10 High       
    ## 11 Low        
    ## 12 Low

``` r

predict(gam_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.256        0.263      0.481
    ##  2     0.256        0.263      0.481
    ##  3     0.153        0.190      0.657
    ##  4     0.542        0.233      0.225
    ##  5     0.182        0.268      0.550
    ##  6     0.184        0.272      0.545
    ##  7     0.436        0.274      0.290
    ##  8     0.297        0.286      0.417
    ##  9     0.217        0.358      0.425
    ## 10     0.127        0.268      0.604
    ## 11     0.549        0.269      0.183
    ## 12     0.399        0.311      0.291

## `ordinal_reg()` models

With the `"polr"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

or_spec <-
  ordinal_reg(ordinal_link = "probit") |>
  set_engine("polr") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   ordinal_link = probit
    ## 
    ## Computational engine: polr

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
or_fit
```

    ## parsnip model object
    ## 
    ## Call:
    ## MASS::polr(formula = Sat ~ Infl + Type + Cont, data = data, method = "probit")
    ## 
    ## Coefficients:
    ##    InflMedium      InflHigh TypeApartment    TypeAtrium   TypeTerrace 
    ##     0.3365528     0.7803485    -0.3459948    -0.2075812    -0.6643415 
    ##      ContHigh 
    ##     0.2205434 
    ## 
    ## Intercepts:
    ##  Low|Medium Medium|High 
    ##  -0.3026072   0.4239680 
    ## 
    ## Residual Deviance: 3455.989 
    ## AIC: 3471.989

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(or_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 Low        
    ##  3 Low        
    ##  4 High       
    ##  5 High       
    ##  6 Low        
    ##  7 Low        
    ##  8 Low        
    ##  9 High       
    ## 10 High       
    ## 11 High       
    ## 12 Low

``` r

predict(or_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.261        0.273      0.465
    ##  2     0.517        0.262      0.221
    ##  3     0.517        0.262      0.221
    ##  4     0.231        0.265      0.504
    ##  5     0.191        0.250      0.559
    ##  6     0.510        0.264      0.226
    ##  7     0.430        0.279      0.291
    ##  8     0.430        0.279      0.291
    ##  9     0.304        0.281      0.416
    ## 10     0.169        0.240      0.591
    ## 11     0.169        0.240      0.591
    ## 12     0.556        0.251      0.193

With the `"ordinalNet"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

or_spec <-
  ordinal_reg(penalty = .001, mixture = .5) |>
  set_engine("ordinalNet", includeLambda0 = TRUE) |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.001
    ##   mixture = 0.5
    ## 
    ## Engine-Specific Arguments:
    ##   includeLambda0 = TRUE
    ## 
    ## Computational engine: ordinalNet

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
or_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Summary of fit:
    ## 
    ##     lambdaVals nNonzero    loglik      devPct      aic      bic
    ## 1  0.226298461        2 -1811.497 0.000000000 3626.994 3637.834
    ## 2  0.177589973        4 -1795.523 0.008817756 3599.047 3620.727
    ## 3  0.139365502        5 -1780.941 0.016867808 3571.882 3598.982
    ## 4  0.109368467        6 -1766.495 0.024842100 3544.991 3577.511
    ## 5  0.085827995        6 -1756.132 0.030562813 3524.265 3556.785
    ## 6  0.067354375        8 -1748.099 0.034997627 3512.197 3555.557
    ## 7  0.052857017        8 -1741.355 0.038720284 3498.710 3542.070
    ## 8  0.041480071        8 -1736.931 0.041162755 3489.861 3533.221
    ## 9  0.032551900        9 -1733.701 0.042945606 3485.402 3534.182
    ## 10 0.025545428        9 -1731.526 0.044146506 3481.051 3529.831
    ## 11 0.020047029        9 -1730.132 0.044915709 3478.264 3527.044
    ## 12 0.015732106        9 -1729.246 0.045404913 3476.492 3525.272
    ## 13 0.012345927        9 -1728.686 0.045714169 3475.372 3524.151
    ## 14 0.009688589        9 -1728.333 0.045908684 3474.667 3523.447
    ## 15 0.007603217        9 -1728.113 0.046030476 3474.226 3523.005
    ## 16 0.005966700        9 -1727.975 0.046106547 3473.950 3522.730
    ## 17 0.004682428        9 -1727.889 0.046153901 3473.778 3522.558
    ## 18 0.003674582        9 -1727.836 0.046183313 3473.672 3522.452
    ## 19 0.002883665        9 -1727.803 0.046201549 3473.606 3522.386
    ## 20 0.002262985        9 -1727.782 0.046212840 3473.565 3522.345
    ## 21 0.000000000       11 -1727.749 0.046231083 3477.499 3537.119

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(or_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 Low        
    ##  3 Low        
    ##  4 High       
    ##  5 High       
    ##  6 Low        
    ##  7 Low        
    ##  8 Low        
    ##  9 High       
    ## 10 High       
    ## 11 High       
    ## 12 Low

``` r

predict(or_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.260        0.275      0.466
    ##  2     0.515        0.261      0.223
    ##  3     0.515        0.261      0.223
    ##  4     0.229        0.264      0.507
    ##  5     0.193        0.246      0.560
    ##  6     0.508        0.264      0.228
    ##  7     0.428        0.282      0.290
    ##  8     0.428        0.282      0.290
    ##  9     0.302        0.284      0.414
    ## 10     0.173        0.233      0.594
    ## 11     0.173        0.233      0.594
    ## 12     0.557        0.248      0.196

With the `"vglm"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

or_spec <-
  ordinal_reg(odds_link = "continuation_ratio") |>
  set_engine("vglm") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   odds_link = continuation_ratio
    ## 
    ## Computational engine: vglm

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
or_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## VGAM::vglm(formula = formula, family = VGAM::cratio(link = "logitlink", 
    ##     parallel = FALSE, Thresh = NULL), data = data)
    ## 
    ## 
    ## Coefficients:
    ##   (Intercept):1   (Intercept):2    InflMedium:1    InflMedium:2      InflHigh:1 
    ##      0.44469787      0.30710093      0.59054309      0.28904209      1.24398695 
    ##      InflHigh:2 TypeApartment:1 TypeApartment:2    TypeAtrium:1    TypeAtrium:2 
    ##      0.95139232     -0.61831533     -0.30826947     -0.15913107     -0.52428810 
    ##   TypeTerrace:1   TypeTerrace:2      ContHigh:1      ContHigh:2 
    ##     -1.07873451     -0.78038044      0.43066868      0.08359363 
    ## 
    ## Degrees of Freedom: 3338 Total; 3324 Residual
    ## Residual deviance: 3445.09 
    ## Log-likelihood: -1722.545

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(or_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 Low        
    ##  3 Low        
    ##  4 High       
    ##  5 High       
    ##  6 Low        
    ##  7 Low        
    ##  8 Low        
    ##  9 High       
    ## 10 High       
    ## 11 High       
    ## 12 Low

``` r

predict(or_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.262        0.262      0.476
    ##  2     0.543        0.228      0.228
    ##  3     0.543        0.228      0.228
    ##  4     0.255        0.208      0.537
    ##  5     0.178        0.267      0.555
    ##  6     0.511        0.267      0.222
    ##  7     0.436        0.270      0.294
    ##  8     0.436        0.270      0.294
    ##  9     0.300        0.286      0.414
    ## 10     0.182        0.215      0.603
    ## 11     0.182        0.215      0.603
    ## 12     0.551        0.268      0.181

## `rand_forest()` models

With the `"ordinalForest"` engine

We’ll model satisfaction of householders under varying conditions.

``` r

library(parsnip)
library(ordered)

house_data <-
  MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]
house_sample <- sort(sample(nrow(house_data), 12L))
house_train <- house_data[-house_sample, ]
house_test <- house_data[house_sample, ]
```

We can define the model with specific parameters:

``` r

rf_spec <-
  rand_forest(trees = 1000) |>
  set_engine("ordinalForest", nsets = 100) |>
  set_mode("classification")
rf_spec
```

    ## Random Forest Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ## 
    ## Engine-Specific Arguments:
    ##   nsets = 100
    ## 
    ## Computational engine: ordinalForest

Now we create the model fit object:

``` r

set.seed(1)
rf_fit <- rf_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
rf_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Ordinal forest 
    ## 
    ## Number of observations: 1669, number of covariates: 3 
    ## 
    ## Classes of ordinal target variable: 
    ## "Low" (n = 563), "Medium" (n = 443), "High" (n = 663) 
    ## 
    ## Forest setup: 
    ## Number of trees in ordinal forest: 1000 
    ## Number of considered score sets in total: 100 
    ## Number of best score sets used for approximating the optimal score set: 10 
    ## Number of trees per regression forests constructed in the optimization: 100 
    ## Performance function: "probability"

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(rf_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 Low        
    ##  3 Low        
    ##  4 High       
    ##  5 High       
    ##  6 Low        
    ##  7 Low        
    ##  8 Low        
    ##  9 High       
    ## 10 High       
    ## 11 High       
    ## 12 Low

``` r

predict(rf_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.335        0.259      0.407
    ##  2     0.471        0.249      0.279
    ##  3     0.471        0.249      0.279
    ##  4     0.285        0.220      0.496
    ##  5     0.276        0.247      0.477
    ##  6     0.399        0.278      0.323
    ##  7     0.399        0.280      0.321
    ##  8     0.399        0.280      0.321
    ##  9     0.279        0.276      0.444
    ## 10     0.214        0.250      0.536
    ## 11     0.214        0.250      0.536
    ## 12     0.489        0.270      0.241
