seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")
  house_sub <- get_house()$sub

  # no extra arguments

  set.seed(seed)
  orig_fit <- VGAM::vglm(
    Sat ~ Type + Infl + Cont,
    family = VGAM::cumulative(parallel = TRUE),
    data = house_sub
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("vglm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE, ignore_formula_env = TRUE
    )
  }

  # extra arguments

  set.seed(seed)
  orig_fit <- VGAM::vglm(
    Sat ~ Type + Infl + Cont,
    # NB: Unused model parameters are ignored without comment.
    family = VGAM::cratio(link = "probitlink", parallel = TRUE),
    data = house_sub
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("vglm") |>
    set_mode("classification") |>
    set_args(ordinal_link = "probit", odds_link = "continuation_ratio")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE, ignore_formula_env = TRUE
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
  house_sub <- get_house()$sub

  set.seed(seed)
  house_wts <- rpois(n = nrow(house_sub), 2) + 1L

  set.seed(seed)
  orig_fit <- VGAM::vglm(
    Sat ~ Type + Infl + Cont,
    family = VGAM::cumulative(parallel = TRUE),
    data = house_sub,
    weights = house_wts
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("vglm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = house_sub,
    case_weights = frequency_weights(house_wts)
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(orig_fit), skip_slots)) {
    expect_equal(
      slot(orig_fit, s),
      slot(tidy_fit$fit, s),
      ignore_attr = TRUE, ignore_formula_env = TRUE
    )
  }
})

# prediction: class ------------------------------------------------------------

# prediction: probability ------------------------------------------------------

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  onet_spec <-
    ordinal_reg() %>%
    set_mode("classification") %>%
    set_engine("vglm")
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
      ignore_attr = TRUE, ignore_formula_env = TRUE
    )
  }
})

test_that("arguments agree", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  onet_arg_spec <-
    ordinal_reg(
      ordinal_link = "cloglog", odds_link = "stopping"
    ) |>
    set_mode("classification") %>%
    set_engine("vglm")
  expect_snapshot(onet_arg_spec %>% translate())

  expect_snapshot({
    set.seed(13)
    onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(onet_arg_fit$fit@family@infos()$link, "clogloglink")
  expect_equal(onet_arg_fit$fit@family@infos()$parallel, TRUE)
  expect_equal(onet_arg_fit$fit@family@vfamily[1L], "sratio")
})
