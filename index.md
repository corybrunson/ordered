# ordered

ordered is a [parsnip](https://parsnip.tidymodels.org/) extension to
enable additional classification models for ordinal outcomes (e.g.,
“low”, “medium”, “high”). While there are several model/engine
combinations in the parsnip package that can be used, this package adds:

- cumulative link (cumulative logit) ordinal regression via
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)
- cumulative link ordinal regression via
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html)
  ([Christensen,
  2023](https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf))
- generalized linear and generalized additive ordinal regression models
  of cumulative link, adjacent categories, continuation ratio, and
  stopping ratio families via
  [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) and
  [`VGAM::vgam()`](https://rdrr.io/pkg/VGAM/man/vgam.html) ([Yee,
  2015](https://doi.org/10.18637/jss.v032.i10))
- regularized elastic net ordinal regression models of cumulative link,
  adjacent categories, continuation ratio, and stopping ratio families
  via
  [`ordinalNet::ordinalNet()`](https://rdrr.io/pkg/ordinalNet/man/ordinalNet.html)
  ([Wurm, Hanlon, and Rathouz,
  2021](https://doi.org/10.18637/jss.v099.i06))
- regularized cumulative probability ordinal regression models via
  [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) and
  [`rms::orm()`](https://rdrr.io/pkg/rms/man/orm.html) ([Harrell,
  2015](https://doi.org/10.1007/978-3-319-19425-7))
- regularized elastic net continuation ratio ordinal regression via
  [`glmnetcr::glmnetcr()`](https://rdrr.io/pkg/glmnetcr/man/glmnetcr.html)
  ([Archer and Williams, 2012](https://doi.org/10.1002/sim.4484))
- ordinal classification trees via
  [`rpartScore::rpartScore()`](https://rdrr.io/pkg/rpartScore/man/rpartScore.html)
  ([Galimberti, Soffritti, and Di Maso,
  2012](https://doi.org/10.18637/jss.v047.i10))
- latent variable ordinal forests via
  [`ordinalForest::ordfor()`](https://rdrr.io/pkg/ordinalForest/man/ordfor.html)
  ([Hornung, 2020](https://doi.org/10.1007/s00357-018-9302-x))
- conditional probability ordered forests via
  [`orf::orf()`](https://rdrr.io/pkg/orf/man/orf.html) ([Lechner and
  Okasa, 2025](https://doi.org/10.1007/s00181-024-02646-4))

Some features in other tidymodels packages are useful for ordinal
outcomes:

- The [partykit](https://cran.r-project.org/package=partykit) engines
  for
  [`parsnip::decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  and
  [`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html),
  provided by [bonsai](https://bonsai.tidymodels.org/), use the ordinal
  nature of ordered factors to train the model, yielding different fits
  than for unordered factors.
- The [yardstick](https://yardstick.tidymodels.org/) package has
  `yardstick::kap()` for weighted and unweighted Kappa statistics (the
  former being of more interest). Also,
  `yardstick::classification_cost()` can utilize more complex cost
  structures and uses the class probabilities for estimation.

## Installation

You can install the latest released version of ordered from CRAN via

``` r

install.packages("ordered")
```

and the development version of ordered from GitHub via

``` r

# install.packages("pak")
pak::pak("corybrunson/ordered")
```

or

``` r

# install.packages("remotes")
remotes::install_github("corybrunson/ordered")
```

## Available models, engines, and prediction types

ordered provides new engines for several models, including all engines
for
[`ordinal_reg()`](https://parsnip.tidymodels.org/reference/ordinal_reg.html),
as summarized in the table. Currently only predictions of
`type = "class"` and `type = "prob"` are supported.

| model              | engine          | class | prob | linear_pred |
|:-------------------|:----------------|:------|:-----|:------------|
| `decision_tree`    | `rpartScore`    | ✔     | ✖    | ✖           |
| `gen_additive_mod` | `vgam`          | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `polr`          | ✔     | ✔    | ✖           |
| `ordinal_reg`      | `clm`           | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `lrm`           | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `orm`           | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `vglm`          | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `ordinalNet`    | ✔     | ✔    | ✔           |
| `ordinal_reg`      | `glmnetcr`      | ✔     | ✔    | ✖           |
| `rand_forest`      | `ordinalForest` | ✔     | ✔    | ✖           |
| `rand_forest`      | `orf`           | ✔     | ✔    | ✖           |

## Example

Here is a simple example using computational chemistry data to predict
the permeability of a molecule:

``` r

library(dplyr)
library(ordered)

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

To voice support for or volunteer to contribute additional engines,
please comment on [this
issue](https://github.com/corybrunson/ordered/issues/15).

## Acknowledgments

Jason Cory Brunson was supported in part by [NIH/NIDCD
R01DC022733](https://reporter.nih.gov/search/oglGXI8zaU2b_BVRqwNHEA/project-details/11122743).
