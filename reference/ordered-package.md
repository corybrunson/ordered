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
#>  1     0.120        0.347      0.533
#>  2     0.514        0.192      0.294
#>  3     0.426        0.328      0.246
#>  4     0.514        0.192      0.294
#>  5     0.227        0.274      0.499
#>  6     0.442        0.326      0.233
#>  7     0.150        0.411      0.439
#>  8     0.231        0.220      0.550
#>  9     0.442        0.326      0.233
#> 10     0.120        0.347      0.533
#> # ℹ 30 more rows
```
