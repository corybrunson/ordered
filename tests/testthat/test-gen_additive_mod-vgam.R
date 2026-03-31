seed <- 144688L

# `VGAM::vgam()` ---------------------------------------------------------------

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  suppressPackageStartupMessages(library(VGAM)) # For s() in gam formula

  # no extra arguments

  set.seed(seed)
  orig_fit <- VGAM::vgam(
    class ~ mol_weight + volume + ClogP,
    family = VGAM::cumulative(parallel = TRUE),
    data = caco_train
  )

  tidy_spec <- gen_additive_mod() |>
    set_engine("vgam") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    class ~ mol_weight + volume + ClogP,
    data = caco_train
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE,
      ignore_formula_env = TRUE
    )
  }

  # extra arguments

  set.seed(seed)
  orig_fit <- VGAM::vgam(
    class ~ s(mol_weight) + volume + ClogP,
    # NB: Unused model parameters are ignored without comment.
    family = VGAM::cratio(link = "probitlink", parallel = TRUE),
    data = caco_train
  )

  tidy_spec <- gen_additive_mod() |>
    set_engine("vgam") |>
    set_mode("classification") |>
    set_args(link = "probit", family = "continuation_ratio")
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    class ~ s(mol_weight) + volume + ClogP,
    data = caco_train
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE,
      ignore_formula_env = TRUE
    )
  }

  # TODO: Add a test using `VGAM::acat()`, which defaults to `link = "loglink"`,
  # once additional links have been enabled.
})

# model: case weights ----------------------------------------------------------

# NB: This test passes when the additional (commented) arguments are passed.
test_that("case weights", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  suppressPackageStartupMessages(library(VGAM)) # For s() in gam formula

  set.seed(seed)
  house_wts <- rpois(n = nrow(caco_train), 2) + 1L

  set.seed(seed)
  orig_fit <- VGAM::vgam(
    class ~ mol_weight + volume + s(ClogP),
    family = VGAM::cumulative(parallel = TRUE),
    data = caco_train,
    weights = house_wts
  )

  tidy_spec <- gen_additive_mod() |>
    set_engine("vgam") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    class ~ mol_weight + volume + s(ClogP),
    data = caco_train,
    case_weights = frequency_weights(house_wts)
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE,
      ignore_formula_env = TRUE
    )
  }
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  suppressPackageStartupMessages(library(VGAM)) # For s() in gam formula

  tidy_fit <- gen_additive_mod(engine = "vgam", mode = "classification") |>
    fit(class ~ s(mol_weight) + volume + s(ClogP), data = caco_train)

  orig_pred <- predict(tidy_fit$fit, newdata = caco_train, type = "response")
  orig_pred <- apply(orig_pred, 1L, which.max)
  orig_pred <- ordered(tidy_fit$lvl[orig_pred], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)

  tidy_pred <- predict(tidy_fit, caco_train, type = "class")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  suppressPackageStartupMessages(library(VGAM)) # For s() in gam formula

  tidy_fit <- gen_additive_mod(engine = "vgam", mode = "classification") |>
    fit(class ~ mol_weight + s(volume) + s(ClogP), data = caco_train)

  orig_pred <- predict(tidy_fit$fit, newdata = caco_train, type = "response")
  orig_pred <- tibble::as_tibble(orig_pred)
  names(orig_pred) <- paste0(".pred_", names(orig_pred))

  tidy_pred <- predict(tidy_fit, caco_train, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  onet_spec <-
    gen_additive_mod() %>%
    set_mode("classification") %>%
    set_engine("vgam")
  expect_snapshot(onet_spec %>% translate())

  expect_no_error({
    set.seed(13)
    onet_f_fit <- fit(onet_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(onet_f_fit)

  expect_no_error({
    set.seed(13)
    onet_xy_fit <- fit_xy(onet_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(onet_xy_fit)

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(onet_f_fit), skip_slots)) {
    expect_equal(
      slot(onet_f_fit$fit, s),
      slot(onet_xy_fit$fit, s),
      ignore_attr = TRUE,
      ignore_formula_env = TRUE
    )
  }
})

test_that("arguments agree", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  onet_arg_spec <-
    gen_additive_mod() |>
    set_mode("classification") %>%
    set_engine("vgam", link = "cloglog", family = "stopping")
  expect_snapshot(onet_arg_spec %>% translate())

  expect_snapshot({
    set.seed(13)
    onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(onet_arg_fit$fit@family@infos()$link, "clogloglink")
  expect_equal(onet_arg_fit$fit@family@infos()$parallel, TRUE)
  expect_equal(onet_arg_fit$fit@family@vfamily[1L], "sratio")
})
