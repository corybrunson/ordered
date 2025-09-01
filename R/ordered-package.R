#' {ordered}: parsnip Engines for Ordinal Regression Models
#'
#' {ordered} provides engines for ordinal regression models for the {parsnip}
#' package. The models may have cumulative, sequential, or adjacent-category
#' structure, and in future these may be disaggregated into separate model
#' types. A vignette will provide thorough illustrations of {ordered}
#' functionality. See below for examples of fitting ordinal regression models
#' with {ordered}.
#'
#' @examples
#' if (rlang::is_installed("MASS")) {
#'
#' # Weighted sample
#'
#' set.seed(561246)
#' house_sub <- MASS::housing |>
#'   dplyr::sample_n(size = 120, replace = TRUE, weight = Freq) |>
#'   subset(select = -Freq)
#' train_inds <- sample(120, 80)
#' house_train <- house_sub[train_inds, ]
#' house_test <- house_sub[-train_inds, ]
#'
#' # Cumulative-link proportional-odds probit regression model
#'
#' fit_cpop <- ordinal_reg() |>
#'   set_engine("polr") |>
#'   set_args(method = "probit") |>
#'   fit(Sat ~ Infl + Type + Cont, data = house_train)
#' predict(fit_cpop, house_test, type = "prob")
#'
#' if (rlang::is_installed("ordinalForest")) {
#'
#' # Ordinal forest
#'
#' fit_orf <- rand_forest(mode = "classification") |>
#'   set_engine("ordinalForest") |>
#'   fit(Sat ~ Infl + Type + Cont, data = house_train)
#' predict(fit_orf, house_test, type = "prob")
#'
#' }
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import parsnip
## usethis namespace: end
NULL
