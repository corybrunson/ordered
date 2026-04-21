# ordered: parsnip Engines for Ordinal Regression Models

ordered provides engines for ordinal regression models for the parsnip
package. The models may have cumulative, sequential, or
adjacent-category structure, and in future these may be disaggregated
into separate model types. A vignette will provide thorough
illustrations of ordered functionality. See below for examples of
fitting ordinal regression models with ordered.

## Author

**Maintainer**: Jason Cory Brunson <cornelioid@gmail.com>
([ORCID](https://orcid.org/0000-0003-3126-9494))

Authors:

- Max Kuhn <max@posit.co>
  ([ORCID](https://orcid.org/0000-0003-2402-136X))

Other contributors:

- Posit Software PBC \[copyright holder\]

## Examples

``` r
if (rlang::is_installed("MASS")) {

# Weighted sample

set.seed(561246)
house_sub <- MASS::housing %>%
  dplyr::sample_n(size = 120, replace = TRUE, weight = Freq) %>%
  subset(select = -Freq)
train_inds <- sample(120, 80)
house_train <- house_sub[train_inds, ]
house_test <- house_sub[-train_inds, ]

# Cumulative-link proportional-odds probit regression model

fit_cpop <- ordinal_reg() %>%
  set_engine("polr") %>%
  set_args(ordinal_link = "probit") %>%
  fit(Sat ~ Infl + Type + Cont, data = house_train)
predict(fit_cpop, house_test, type = "prob")

if (rlang::is_installed("ordinalForest")) {

# Ordinal forest

fit_orf <- rand_forest(mode = "classification") %>%
  set_engine("ordinalForest") %>%
  set_args(nsets = 50, ntreefinal = 100, perffunction = "probability") %>%
  fit(Sat ~ Infl + Type + Cont, data = house_train)
predict(fit_orf, house_test, type = "prob")

}
}
#> Warning: The argument `ntreefinal` cannot be manually modified and was removed.
#> # A tibble: 40 × 3
#>    .pred_Low .pred_Medium .pred_High
#>        <dbl>        <dbl>      <dbl>
#>  1     0.123        0.343      0.534
#>  2     0.514        0.193      0.292
#>  3     0.422        0.328      0.250
#>  4     0.514        0.193      0.292
#>  5     0.232        0.276      0.492
#>  6     0.436        0.328      0.236
#>  7     0.155        0.412      0.434
#>  8     0.239        0.218      0.544
#>  9     0.436        0.328      0.236
#> 10     0.123        0.343      0.534
#> # ℹ 30 more rows
```
