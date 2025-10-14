
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ordered

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ordered)](https://CRAN.R-project.org/package=ordered)
[![R-CMD-check](https://github.com/corybrunson/ordered/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/corybrunson/ordered/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/corybrunson/ordered/branch/main/graph/badge.svg)](https://app.codecov.io/gh/corybrunson/ordered?branch=main)
<!-- badges: end -->

`ordered` is a [parsnip](https://parsnip.tidymodels.org/) extension to
enable additional classification models for ordinal outcomes (e.g.,
“low”, “medium”, “high”). While there are several model/engine
combinations in the parsnip package that can be used, this package adds:

- cumulative link (cumulative logit) ordinal regression via
  `MASS::polr()`
- regularized elastic net ordinal regression via
  `ordinalNet::ordinalNet()` ([Wurm, Hanlon, and Rathouz,
  2021](https://doi.org/10.18637/jss.v099.i06))
- ordinal classification trees via `rpartScore::rpartScore()`
  ([Galimberti, Soffritti, and Di Maso,
  2012](https://doi.org/10.18637/jss.v047.i10))
- latent variable ordinal forests via `ordinalForest::ordfor()`
  ([Hornung, 2020](https://doi.org/10.1007/s00357-018-9302-x))

More will be added. Under consideration are:

- generalized linear and generalized additive ordinal regression models
  of cumulative link, adjacent categories, continuation ratio, and
  stopping ratio families via `VGAM::vglm()` and `VGAM::vgam()` ([Yee,
  2015](https://doi.org/10.18637/jss.v032.i10))
- regularized cumulative probability (cumulative logit) ordinal
  regression via `rms::lrm()` and `rms::orm()` ([Harrell,
  2015](https://doi.org/10.1007/978-3-319-19425-7))
- continuation ratio (stopping ratio) ordinal regression using elastic
  net regularization via `glmnetcr::glmnetcr()` and using regularization
  path computation via `glmpathcr::glmpathcr()` ([Archer and Williams,
  2012](https://doi.org/10.1002/sim.4484))
- generalized monotone incremental forward stagewise regularized ordinal
  regression via `ordinalgmifs::ordinalgmifs()` ([Archer, Hou, Zhou,
  Ferber, Layne, and Gentry, 2014](https://doi.org/10.4137/CIN.S20806);
  [Gentry, Jackson-Cook, Lyon, and Archer,
  2015](https://doi.org/10.4137/CIN.S17277))
- Bayesian LASSO ordinal regression via `ordinalbayes::ordinalbayes()`
  ([Zhang and Archer,
  2021](https://doi.org/10.1186%2Fs12859-021-04432-w))
- cumulative link ordinal regression via `ordinal::clm()` ([Christensen,
  2023](https://cran.uni-muenster.de/web/packages/ordinal/vignettes/clm_article.pdf))
- conditional probability ordered forests via `orf::orf()` ([Lechner and
  Okasa, 2025](https://doi.org/10.1007/s00181-024-02646-4))

There are some existing features in tidymodels packages that are useful
for ordinal outcomes:

- The [partykit](https://cran.r-project.org/package=partykit) engines
  for `parsnip::decision_tree()` and `parsnip::rand_forest()` use the
  ordered nature of the factors to train the model.
- The yardstick package has `yardstick::kap()` for weighted and
  unweighted Kappa statistics (the former being of more interest). Also,
  `yardstick::classification_cost()` can utilize more complex cost
  structures and uses the class probabilities for estimation.

## Installation

You can install the development version of ordered like so:

``` r
# install.packages("pak")
pak::pak("corybrunson/ordered", dependencies = FALSE)
```

Currently, ordered relies on engine and dial registration in the
following forks:

``` r
pak::pak("corybrunson/parsnip@ordered", dependencies = FALSE)
pak::pak("corybrunson/dials@ordered", dependencies = FALSE)
```

## Example

Here is a simple example using computational chemistry data to predict
the permeability of a molecule:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ordered)
#> Loading required package: parsnip

data(caco, package = "QSARdata")

caco_dat <-
  inner_join(caco_Outcome, caco_Dragon, by = "Molecule") %>%
  as_tibble() %>%
  select(
    class = Class,
    mol_weight = QikProp_mol_MW,
    volume = QikProp_volume,
    ClogP
  )
caco_train <- caco_dat[-(1:10), ]
caco_test  <- caco_dat[ (1:10), ]

ord_rf_spec <- 
  # you should really use many more trees and score sets
  rand_forest(mtry = 2, trees = 100) %>%
  set_mode("classification") %>%
  set_engine("ordinalForest", nsets = 100)

set.seed(382)
ord_rf_fit <- ord_rf_spec %>% fit(class ~ ., data = caco_train)
augment(ord_rf_fit, new_data = caco_test)
#> # A tibble: 10 × 8
#>    .pred_class .pred_L .pred_M .pred_H class mol_weight volume  ClogP
#>    <ord>         <dbl>   <dbl>   <dbl> <ord>      <dbl>  <dbl>  <dbl>
#>  1 L            0.374    0.368  0.258  M           123.   445.  0.799
#>  2 M            0.289    0.512  0.199  L           290.   856.  0.534
#>  3 M            0.170    0.809  0.0212 M           519.  1576.  1.02 
#>  4 M            0.209    0.740  0.0503 M           533.  1606.  1.58 
#>  5 M            0.126    0.762  0.113  M           505.  1517.  1.71 
#>  6 M            0.0657   0.917  0.0176 M           519.  1547.  2.27 
#>  7 M            0.213    0.747  0.0401 M           517.  1600.  1.78 
#>  8 M            0.111    0.859  0.0306 M           531.  1631.  2.34 
#>  9 M            0.0403   0.932  0.0277 M           517.  1572.  2.81 
#> 10 L            0.569    0.428  0.003  L           588.  1799. -1.85
```

## Code of Conduct

Please note that the ordered project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
