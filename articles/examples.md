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
    ##     parallel = TRUE), data = data)
    ## 
    ## 
    ## Degrees of Freedom: 3338 Total; 3330 Residual
    ## Residual deviance: 3460.191 
    ## Log-likelihood: -1730.096

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
    ##  1     0.262        0.256      0.481
    ##  2     0.262        0.256      0.481
    ##  3     0.158        0.185      0.657
    ##  4     0.490        0.301      0.209
    ##  5     0.212        0.226      0.562
    ##  6     0.211        0.226      0.563
    ##  7     0.420        0.302      0.278
    ##  8     0.306        0.276      0.418
    ##  9     0.277        0.264      0.459
    ## 10     0.168        0.193      0.638
    ## 11     0.532        0.295      0.173
    ## 12     0.409        0.301      0.290

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
    ## MASS::polr(formula = Sat ~ Infl + Type + Cont, data = data, method = ~"probit")
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
  set_engine("ordinalNet") |>
  set_mode("classification")
or_spec
```

    ## Ordinal Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.001
    ##   mixture = 0.5
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
    ##       lambdaVals nNonzero    loglik     devPct      aic      bic
    ## 1   2.262985e-01        2 -1811.497 0.00000000 3626.994 3637.834
    ## 2   1.938450e-01        4 -1800.934 0.00583091 3609.868 3631.548
    ## 3   1.660458e-01        4 -1791.815 0.01086503 3591.630 3613.310
    ## 4   1.422332e-01        5 -1782.190 0.01617832 3574.380 3601.480
    ## 5   1.218356e-01        6 -1772.588 0.02147857 3557.177 3589.697
    ## 6   1.043631e-01        6 -1764.162 0.02612998 3540.325 3572.845
    ## 7   8.939645e-02        6 -1757.599 0.02975313 3527.198 3559.718
    ## 8   7.657612e-02        6 -1752.516 0.03255915 3517.032 3549.552
    ## 9   6.559436e-02        8 -1747.219 0.03548354 3510.437 3553.797
    ## 10  5.618748e-02        8 -1742.800 0.03792292 3501.599 3544.959
    ## 11  4.812965e-02        8 -1739.418 0.03978953 3494.836 3538.196
    ## 12  4.122739e-02        8 -1736.842 0.04121183 3489.683 3533.043
    ## 13  3.531498e-02        9 -1734.675 0.04240808 3487.349 3536.129
    ## 14  3.025046e-02        9 -1732.939 0.04336623 3483.878 3532.658
    ## 15  2.591225e-02        9 -1731.629 0.04408961 3481.257 3530.037
    ## 16  2.219618e-02        9 -1730.643 0.04463396 3479.285 3528.065
    ## 17  1.901303e-02        9 -1729.903 0.04504236 3477.805 3526.585
    ## 18  1.628638e-02        9 -1729.349 0.04534793 3476.698 3525.478
    ## 19  1.395075e-02        9 -1728.936 0.04557601 3475.872 3524.652
    ## 20  1.195008e-02        9 -1728.628 0.04574580 3475.257 3524.037
    ## 21  1.023632e-02        9 -1728.400 0.04587209 3474.799 3523.579
    ## 22  8.768330e-03        9 -1728.230 0.04596581 3474.460 3523.240
    ## 23  7.510866e-03        9 -1728.104 0.04603525 3474.208 3522.988
    ## 24  6.433734e-03        9 -1728.011 0.04608663 3474.022 3522.802
    ## 25  5.511074e-03        9 -1727.942 0.04612461 3473.885 3522.664
    ## 26  4.720732e-03        9 -1727.891 0.04615265 3473.783 3522.563
    ## 27  4.043733e-03        9 -1727.854 0.04617334 3473.708 3522.488
    ## 28  3.463822e-03        9 -1727.826 0.04618859 3473.653 3522.433
    ## 29  2.967076e-03        9 -1727.806 0.04619983 3473.612 3522.392
    ## 30  2.541568e-03        9 -1727.791 0.04620810 3473.582 3522.362
    ## 31  2.177082e-03        9 -1727.780 0.04621419 3473.560 3522.340
    ## 32  1.864868e-03        9 -1727.772 0.04621867 3473.544 3522.324
    ## 33  1.597427e-03        9 -1727.766 0.04622196 3473.532 3522.312
    ## 34  1.368341e-03        9 -1727.762 0.04622439 3473.523 3522.303
    ## 35  1.172107e-03        9 -1727.758 0.04622616 3473.517 3522.296
    ## 36  1.004016e-03        9 -1727.756 0.04622747 3473.512 3522.292
    ## 37  8.600299e-04        9 -1727.754 0.04622843 3473.508 3522.288
    ## 38  7.366932e-04        9 -1727.753 0.04622914 3473.506 3522.286
    ## 39  6.310442e-04        9 -1727.752 0.04622965 3473.504 3522.284
    ## 40  5.405463e-04        9 -1727.751 0.04623003 3473.503 3522.282
    ## 41  4.630266e-04        9 -1727.751 0.04623031 3473.502 3522.281
    ## 42  3.966241e-04        9 -1727.750 0.04623052 3473.501 3522.281
    ## 43  3.397443e-04        9 -1727.750 0.04623067 3473.500 3522.280
    ## 44  2.910216e-04        9 -1727.750 0.04623078 3473.500 3522.280
    ## 45  2.492863e-04        9 -1727.750 0.04623086 3473.500 3522.279
    ## 46  2.135362e-04        9 -1727.750 0.04623092 3473.499 3522.279
    ## 47  1.829130e-04        9 -1727.750 0.04623096 3473.499 3522.279
    ## 48  1.566815e-04        9 -1727.750 0.04623100 3473.499 3522.279
    ## 49  1.342118e-04        9 -1727.749 0.04623102 3473.499 3522.279
    ## 50  1.149646e-04        9 -1727.749 0.04623104 3473.499 3522.279
    ## 51  9.847751e-05        9 -1727.749 0.04623105 3473.499 3522.279
    ## 52  8.435487e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 53  7.225756e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 54  6.189512e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 55  5.301875e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 56  4.541534e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 57  3.890234e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 58  3.332336e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 59  2.854447e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 60  2.445091e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 61  2.094441e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 62  1.794078e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 63  1.536789e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 64  1.316399e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 65  1.127614e-05        9 -1727.749 0.04623106 3473.499 3522.279
    ## 66  9.659034e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 67  8.273834e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 68  7.087285e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 69  6.070899e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 70  5.200273e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 71  4.454503e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 72  3.815684e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 73  3.268477e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 74  2.799746e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 75  2.398235e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 76  2.054304e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 77  1.759697e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 78  1.507339e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 79  1.291172e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 80  1.106005e-06        9 -1727.749 0.04623106 3473.499 3522.279
    ## 81  9.473934e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 82  8.115279e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 83  6.951468e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 84  5.954560e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 85  5.100618e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 86  4.369139e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 87  3.742562e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 88  3.205842e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 89  2.746093e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 90  2.352276e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 91  2.014937e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 92  1.725975e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 93  1.478453e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 94  1.266429e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 95  1.084810e-07        9 -1727.749 0.04623106 3473.499 3522.279
    ## 96  9.292380e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 97  7.959762e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 98  6.818254e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 99  5.840450e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 100 5.002872e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 101 4.285411e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 102 3.670842e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 103 3.144407e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 104 2.693468e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 105 2.307199e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 106 1.976324e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 107 1.692900e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 108 1.450121e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 109 1.242160e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 110 1.064022e-08        9 -1727.749 0.04623106 3473.499 3522.279
    ## 111 9.114306e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 112 7.807226e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 113 6.687593e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 114 5.728527e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 115 4.907000e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 116 4.203288e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 117 3.600495e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 118 3.084149e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 119 2.641852e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 120 2.262985e-09        9 -1727.749 0.04623106 3473.499 3522.279
    ## 121 0.000000e+00        9 -1727.749 0.04623106 3473.499 3522.279

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
    ##     parallel = TRUE), data = data)
    ## 
    ## 
    ## Coefficients:
    ## (Intercept):1 (Intercept):2    InflMedium      InflHigh TypeApartment 
    ##     0.5357923     0.1421721     0.4738798     1.1288464    -0.4913145 
    ##    TypeAtrium   TypeTerrace      ContHigh 
    ##    -0.3361781    -0.9588670     0.2834405 
    ## 
    ## Degrees of Freedom: 3338 Total; 3330 Residual
    ## Residual deviance: 3459.711 
    ## Log-likelihood: -1729.856

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
    ##  1     0.267        0.257      0.476
    ##  2     0.489        0.300      0.211
    ##  3     0.489        0.300      0.211
    ##  4     0.236        0.240      0.524
    ##  5     0.209        0.223      0.568
    ##  6     0.487        0.300      0.213
    ##  7     0.419        0.300      0.281
    ##  8     0.419        0.300      0.281
    ##  9     0.310        0.276      0.415
    ## 10     0.189        0.208      0.603
    ## 11     0.189        0.208      0.603
    ## 12     0.535        0.293      0.172

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
