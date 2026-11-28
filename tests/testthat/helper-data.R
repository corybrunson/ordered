# https://testthat.r-lib.org/articles/skipping.html#helpers

seed <- 144688L

if (rlang::is_installed("QSARdata")) {
  library(dplyr)
  data(caco, package = "QSARdata", envir = rlang::current_env())

  set.seed(1)
  caco_dat <-
    dplyr::inner_join(caco_Outcome, caco_Dragon, by = "Molecule") |>
    dplyr::as_tibble() |>
    dplyr::select(
      class = Class,
      mol_weight = QikProp_mol_MW,
      volume = QikProp_volume,
      ClogP
    ) |>
    dplyr::slice_sample(n = 20, by = class)
  caco_train <- caco_dat[-c(1:2, 21:22, 41:42), ]
  caco_test  <- caco_dat[ c(1:2, 21:22, 41:42), ]
}

get_house <- function() {
  require(MASS)
  set.seed(581837)
  house_data <- MASS::housing
  house_sub <-
    house_data[sample(72, 120, replace = TRUE, prob = house_data$Freq), ] |>
    subset(select = -Freq)
  list(data = house_data, sub = house_sub)
}

get_ordinalgmifs_ex <- function() {
  set.seed(seed)
  d <- data.frame(
    y = factor(
      rep(c("low", "medium", "high"), each = 12),
      levels = c("low", "medium", "high")
    ),
    x1 = rnorm(36),
    x2 = rnorm(36),
    z = rnorm(36)
  )
  x <- as.matrix(d[, c("x1", "x2"), drop = FALSE])
  rownames(x) <- NULL
  list(data = d, x = x)

}
