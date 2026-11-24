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
  decision_tree(tree_depth = 3) |>
  set_engine("rpartScore", split = "quad") |>
  set_mode("classification")
dt_spec
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   tree_depth = 3
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
    ## Residual deviance: 3443.78 
    ## Log-likelihood: -1721.89

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

``` r

predict(gam_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 Low        
    ##  3 Low        
    ##  4 Low        
    ##  5 High       
    ##  6 Low        
    ##  7 High       
    ##  8 High       
    ##  9 Low        
    ## 10 Low        
    ## 11 High       
    ## 12 Low

``` r

predict(gam_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.256        0.260      0.483
    ##  2     0.390        0.258      0.352
    ##  3     0.390        0.258      0.352
    ##  4     0.390        0.258      0.352
    ##  5     0.256        0.210      0.534
    ##  6     0.512        0.264      0.224
    ##  7     0.361        0.247      0.392
    ##  8     0.108        0.187      0.705
    ##  9     0.435        0.273      0.292
    ## 10     0.435        0.273      0.292
    ## 11     0.293        0.285      0.422
    ## 12     0.558        0.265      0.177

``` r

predict(gam_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1          0.616   
    ##  2          0.000272
    ##  3          0.000272
    ##  4          0.000272
    ##  5          0.617   
    ##  6         -0.495   
    ##  7          0.121   
    ##  8          1.66    
    ##  9         -0.186   
    ## 10         -0.186   
    ## 11          0.430   
    ## 12         -0.681

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

With the `"clm"` engine

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
  ordinal_reg(ordinal_link = "Aranda-Ordaz") |>
  set_engine("clm") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   ordinal_link = Aranda-Ordaz
    ## 
    ## Computational engine: clm

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
```

    ## Changing to 'nlminb' optimizer for flexible link function

    ## Warning: (-1) Model failed to converge with max|grad| = 0.00293519 (tol =
    ## 1e-06)

``` r

or_fit
```

    ## parsnip model object
    ## 
    ## formula: Sat ~ Infl + Type + Cont
    ## data:    data
    ## 
    ##  link         threshold nobs logLik   AIC     niter   max.grad cond.H 
    ##  Aranda-Ordaz flexible  1669 -1727.63 3473.26 50(494) 2.94e-03 3.2e+02
    ## 
    ## Coefficients:
    ##    InflMedium      InflHigh TypeApartment    TypeAtrium   TypeTerrace 
    ##        0.5169        1.2131       -0.5375       -0.3361       -1.0251 
    ##      ContHigh 
    ##        0.3273 
    ## 
    ## Link coefficient:
    ## lambda 
    ## 0.7958 
    ## 
    ## Threshold coefficients:
    ##  Low|Medium Medium|High 
    ##     -0.5626      0.5529

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

``` r

predict(or_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <fct>      
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
    ##  1     0.260        0.271      0.470
    ##  2     0.514        0.268      0.217
    ##  3     0.514        0.268      0.217
    ##  4     0.230        0.258      0.512
    ##  5     0.195        0.240      0.565
    ##  6     0.506        0.271      0.223
    ##  7     0.428        0.286      0.286
    ##  8     0.428        0.286      0.286
    ##  9     0.304        0.283      0.414
    ## 10     0.176        0.227      0.597
    ## 11     0.176        0.227      0.597
    ## 12     0.557        0.255      0.188

``` r

predict(or_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1             0.517
    ##  2            -0.537
    ##  3            -0.537
    ##  4             0.676
    ##  5             0.877
    ##  6            -0.508
    ##  7            -0.210
    ##  8            -0.210
    ##  9             0.307
    ## 10             1.00 
    ## 11             1.00 
    ## 12            -0.698

With the `"lrm"` engine

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
  ordinal_reg(penalty = .001) |>
  set_engine("lrm") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.001
    ## 
    ## Computational engine: lrm

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
or_fit
```

    ## parsnip model object
    ## 
    ## Logistic Regression Model
    ## 
    ## rms::lrm(formula = Sat ~ Infl + Type + Cont, data = data, penalty = 0.001)
    ## 
    ## 
    ## Penalty factors
    ## 
    ##  simple nonlinear interaction nonlinear.interaction
    ##   0.001     0.001       0.001                 0.001
    ## 
    ##                       Model Likelihood       Discrimination    Rank Discrim.    
    ##                             Ratio Test              Indexes          Indexes    
    ## Obs           1669    LR chi2   167.49       R2       0.108    C       0.650    
    ##  Low           563    d.f.           6      R2(6,1669)0.092    Dxy     0.299    
    ##  Medium        443    P(> chi2)<0.0001    R2(6,1469.1)0.104    gamma   0.316    
    ##  High          663    Penalty     0.00       Brier    0.207    tau-a   0.197    
    ## max |deriv| 0.0001                                                              
    ## 
    ##                Coef    S.E.   Wald Z P(>|Z|) Penalty Scale
    ## y>=Medium       0.5000 0.1251  4.00  <0.0001 0.0000       
    ## y>=High        -0.6867 0.1257 -5.46  <0.0001 0.0000       
    ## Infl=Medium     0.5507 0.1049  5.25  <0.0001 0.0258       
    ## Infl=High       1.2851 0.1278 10.06  <0.0001 0.0258       
    ## Type=Apartment -0.5696 0.1195 -4.77  <0.0001 0.0274       
    ## Type=Atrium    -0.3499 0.1553 -2.25  0.0243  0.0274       
    ## Type=Terrace   -1.0901 0.1518 -7.18  <0.0001 0.0274       
    ## Cont=High       0.3573 0.0959  3.73  0.0002  0.0224

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

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
    ##  1     0.259        0.275      0.466
    ##  2     0.517        0.261      0.222
    ##  3     0.517        0.261      0.222
    ##  4     0.229        0.264      0.507
    ##  5     0.192        0.246      0.562
    ##  6     0.510        0.263      0.227
    ##  7     0.429        0.282      0.289
    ##  8     0.429        0.282      0.289
    ##  9     0.302        0.284      0.414
    ## 10     0.172        0.233      0.595
    ## 11     0.172        0.233      0.595
    ## 12     0.558        0.247      0.195

``` r

predict(or_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1             0.551
    ##  2            -0.570
    ##  3            -0.570
    ##  4             0.715
    ##  5             0.935
    ##  6            -0.539
    ##  7            -0.212
    ##  8            -0.212
    ##  9             0.338
    ## 10             1.07 
    ## 11             1.07 
    ## 12            -0.733

With the `"orm"` engine

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
  ordinal_reg(ordinal_link = "cauchit", penalty = .001) |>
  set_engine("orm") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   ordinal_link = cauchit
    ##   penalty = 0.001
    ## 
    ## Computational engine: orm

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
or_fit
```

    ## parsnip model object
    ## 
    ## Cauchy Ordinal Regression Model
    ## 
    ## rms::orm(formula = Sat ~ Infl + Type + Cont, data = data, family = "cauchit", 
    ##     penalty = 0.001)
    ## 
    ## 
    ## Penalty factors
    ## 
    ##  simple nonlinear interaction nonlinear.interaction
    ##   0.001     0.001       0.001                 0.001
    ## 
    ##                       Model Likelihood              Discrimination    Rank Discrim.    
    ##                             Ratio Test                     Indexes          Indexes    
    ## Obs           1669    LR chi2   162.10    R2                 0.104    rho     0.311    
    ##  Low           563    d.f.           6    R2(6,1669)         0.089    Dxy     0.298    
    ##  Medium        443    P(> chi2)<0.0001    R2(6,1469.1)       0.101                     
    ##  High          663    Score chi2177.32    |P(Y>=median)-0.5| 0.174                     
    ## ESS         1469.1    P(> chi2)<0.0001                                                 
    ## Distinct Y       3    Penalty     0.00                                                 
    ## Median Y         2                                                                     
    ## max |deriv|  2e-09                                                                     
    ## 
    ##                Coef    S.E.   Wald Z P(>|Z|) Penalty Scale
    ## y>=Medium       0.4656 0.1125  4.14  <0.0001 0.0000       
    ## y>=High        -0.5963 0.1143 -5.22  <0.0001 0.0000       
    ## Infl=Medium     0.4938 0.0944  5.23  <0.0001 0.0258       
    ## Infl=High       1.1229 0.1193  9.41  <0.0001 0.0258       
    ## Type=Apartment -0.4964 0.1087 -4.57  <0.0001 0.0274       
    ## Type=Atrium    -0.3464 0.1352 -2.56  0.0104  0.0274       
    ## Type=Terrace   -0.9260 0.1361 -6.81  <0.0001 0.0274       
    ## Cont=High       0.2802 0.0846  3.31  0.0009  0.0224

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

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
    ##  1     0.257        0.276      0.467
    ##  2     0.510        0.254      0.236
    ##  3     0.510        0.254      0.236
    ##  4     0.236        0.254      0.510
    ##  5     0.216        0.228      0.557
    ##  6     0.489        0.265      0.246
    ##  7     0.422        0.295      0.283
    ##  8     0.422        0.295      0.283
    ##  9     0.297        0.302      0.402
    ## 10     0.200        0.204      0.596
    ## 11     0.200        0.204      0.596
    ## 12     0.557        0.227      0.216

``` r

predict(or_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1             0.494
    ##  2            -0.496
    ##  3            -0.496
    ##  4             0.626
    ##  5             0.776
    ##  6            -0.432
    ##  7            -0.216
    ##  8            -0.216
    ##  9             0.278
    ## 10             0.907
    ## 11             0.907
    ## 12            -0.646

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

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

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

``` r

predict(or_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1            -0.591
    ##  2             0.618
    ##  3             0.618
    ##  4            -0.626
    ##  5            -1.08 
    ##  6             0.488
    ##  7             0.188
    ##  8             0.188
    ##  9            -0.403
    ## 10            -1.06 
    ## 11            -1.06 
    ## 12             0.648

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

The holdout data can be predicted for the most likely class, all class
probabilities, or the linear predictor:

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

``` r

predict(or_fit, house_test, type = "linear_pred")
```

    ## # A tibble: 12 × 1
    ##    .pred_linear_pred
    ##                <dbl>
    ##  1             0.369
    ##  2            -0.727
    ##  3            -0.727
    ##  4             0.538
    ##  5             0.744
    ##  6            -0.698
    ##  7            -0.382
    ##  8            -0.382
    ##  9             0.161
    ## 10             0.883
    ## 11             0.883
    ## 12            -0.896

With the `"glmnetcr"` engine

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
  set_engine("glmnetcr") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.001
    ##   mixture = 0.5
    ## 
    ## Computational engine: glmnetcr

Now we create the model fit object:

``` r

set.seed(1)
or_fit <- or_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
```

    ## Warning in .resolve_control(control = control, nvars = nvars, deprecated =
    ## list(thresh = if (!missing(thresh)) thresh, : Passing 'thresh' to glmnet() is
    ## deprecated. Use control = list(thresh = ...) instead.

    ## Warning in .resolve_control(control = control, nvars = nvars, deprecated =
    ## list(thresh = if (!missing(thresh)) thresh, : Passing 'maxit' to glmnet() is
    ## deprecated. Use control = list(maxit = ...) instead.

    ## Warning in .resolve_control(control = control, nvars = nvars, deprecated =
    ## list(thresh = if (!missing(thresh)) thresh, : Passing 'dfmax' to glmnet() is
    ## deprecated. Use control = list(dfmax = ...) instead.

    ## Warning in .resolve_control(control = control, nvars = nvars, deprecated =
    ## list(thresh = if (!missing(thresh)) thresh, : Passing 'pmax' to glmnet() is
    ## deprecated. Use control = list(pmax = ...) instead.

    ## Warning in .resolve_control(control = control, nvars = nvars, deprecated =
    ## list(thresh = if (!missing(thresh)) thresh, : Passing 'trace.it' to glmnet() is
    ## deprecated. Use control = list(trace.it = ...) instead.

``` r

or_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:  glmnet(x = glmnet.data$x, y = glmnet.data$y, family = "binomial",      weights = glmnet.data$weights, offset = offset, alpha = alpha,      nlambda = nlambda, lambda.min.ratio = lambda.min.ratio, lambda = lambda,      standardize = standardize, thresh = thresh, dfmax = dfmax,      pmax = pmax, exclude = exclude, penalty.factor = c(penalty.factor,          rep(0, k - 1)), maxit = maxit, type.gaussian = ifelse(nvars <          500, "covariance", "naive"), type.logistic = type.logistic,      trace.it = trace.it) 
    ## 
    ##       Df     %Dev   Lambda
    ##  [1,]  2 0.001323 0.128900
    ##  [2,]  4 0.004632 0.117500
    ##  [3,]  4 0.007936 0.107000
    ##  [4,]  4 0.010790 0.097520
    ##  [5,]  4 0.013270 0.088860
    ##  [6,]  5 0.016280 0.080970
    ##  [7,]  6 0.019590 0.073770
    ##  [8,]  6 0.022540 0.067220
    ##  [9,]  6 0.025060 0.061250
    ## [10,]  6 0.027220 0.055810
    ## [11,]  6 0.029070 0.050850
    ## [12,]  8 0.030740 0.046330
    ## [13,]  8 0.032670 0.042220
    ## [14,]  8 0.034460 0.038470
    ## [15,]  8 0.035980 0.035050
    ## [16,]  8 0.037270 0.031930
    ## [17,]  9 0.038410 0.029100
    ## [18,]  9 0.039460 0.026510
    ## [19,]  9 0.040370 0.024160
    ## [20,]  9 0.041150 0.022010
    ## [21,]  9 0.041810 0.020060
    ## [22,]  9 0.042360 0.018270
    ## [23,]  9 0.042830 0.016650
    ## [24,]  9 0.043230 0.015170
    ## [25,]  9 0.043560 0.013820
    ## [26,]  9 0.043840 0.012600
    ## [27,]  9 0.044070 0.011480
    ## [28,]  9 0.044270 0.010460
    ## [29,]  9 0.044430 0.009528
    ## [30,]  9 0.044570 0.008682
    ## [31,]  9 0.044690 0.007910
    ## [32,]  9 0.044780 0.007208
    ## [33,]  9 0.044870 0.006567
    ## [34,]  9 0.044930 0.005984
    ## [35,]  9 0.044990 0.005452
    ## [36,]  9 0.045040 0.004968
    ## [37,]  9 0.045080 0.004527
    ## [38,]  9 0.045110 0.004125
    ## [39,]  9 0.045140 0.003758
    ## [40,]  9 0.045160 0.003424
    ## [41,]  9 0.045180 0.003120
    ## [42,]  9 0.045190 0.002843
    ## [43,]  9 0.045210 0.002590
    ## [44,]  9 0.045220 0.002360
    ## [45,]  9 0.045230 0.002151

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
    ##  1     0.261        0.289      0.450
    ##  2     0.525        0.230      0.245
    ##  3     0.525        0.230      0.245
    ##  4     0.230        0.289      0.481
    ##  5     0.181        0.283      0.536
    ##  6     0.514        0.234      0.251
    ##  7     0.430        0.261      0.309
    ##  8     0.430        0.261      0.309
    ##  9     0.299        0.287      0.414
    ## 10     0.161        0.278      0.561
    ## 11     0.161        0.278      0.561
    ## 12     0.553        0.220      0.227

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

With the `"orf"` engine

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
  set_engine("orf", sample.fraction = 1) |>
  set_mode("classification")
rf_spec
```

    ## Random Forest Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   trees = 1000
    ## 
    ## Engine-Specific Arguments:
    ##   sample.fraction = 1
    ## 
    ## Computational engine: orf

Now we create the model fit object:

``` r

set.seed(1)
rf_fit <- rf_spec |> fit(Sat ~ Infl + Type + Cont, data = house_train)
rf_fit
```

    ## parsnip model object
    ## 
    ## Ordered Forest object of class orf 
    ## 
    ## Number of Categories:             3 
    ## Sample Size:                      1669 
    ## Number of Trees:                  1000 
    ## Build:                            Subsampling 
    ## Mtry:                             3 
    ## Minimum Node Size:                5 
    ## Honest Forest:                    TRUE 
    ## Weight-Based Inference:           FALSE

The holdout data can be predicted for the most likely class or all class
probabilities:

``` r

predict(rf_fit, house_test, type = "class")
```

    ## # A tibble: 12 × 1
    ##    .pred_class
    ##    <ord>      
    ##  1 High       
    ##  2 High       
    ##  3 Medium     
    ##  4 Low        
    ##  5 High       
    ##  6 High       
    ##  7 High       
    ##  8 Low        
    ##  9 High       
    ## 10 High       
    ## 11 Medium     
    ## 12 Low

``` r

predict(rf_fit, house_test, type = "prob")
```

    ## # A tibble: 12 × 3
    ##    .pred_Low .pred_Medium .pred_High
    ##        <dbl>        <dbl>      <dbl>
    ##  1     0.342        0.285      0.372
    ##  2     0.342        0.285      0.372
    ##  3     0.336        0.373      0.291
    ##  4     0.442        0.298      0.260
    ##  5     0.308        0.260      0.432
    ##  6     0.235        0.256      0.509
    ##  7     0.235        0.256      0.509
    ##  8     0.499        0.256      0.246
    ##  9     0.316        0.234      0.450
    ## 10     0.316        0.234      0.450
    ## 11     0.289        0.381      0.330
    ## 12     0.465        0.318      0.218
