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

  tidy_spec <- ordinal_reg(parallel_reg = TRUE) |>
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
    family = VGAM::cratio(
      link = "probitlink", parallel = TRUE, Thresh = "symm1"
    ),
    data = house_sub
  )

  tidy_spec <- ordinal_reg(parallel_reg = TRUE) |>
    set_engine("vglm") |>
    set_mode("classification") |>
    set_args(
      ordinal_link = "probit", odds_link = "continuation_ratio",
      threshold_structure = "symmetric_median"
    )
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

  tidy_spec <- ordinal_reg(parallel_reg = TRUE) |>
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

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "vglm", parallel_reg = TRUE) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  # as in `parsnip::set_pred()`, use `VGAM::predictvglm()` to avoid mis-dispatch
  # when 'VGAM' is not attached and `predict()` calls `stats::predict()`
  orig_pred <- VGAM::predictvglm(
    tidy_fit$fit,
    newdata = house_sub,
    type = "response"
  )
  orig_pred <- apply(orig_pred, 1L, which.max)
  orig_pred <- ordered(tidy_fit$lvl[orig_pred], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)

  tidy_pred <- predict(tidy_fit, house_sub, type = "class")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "vglm", parallel_reg = TRUE) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  # as in `parsnip::set_pred()`, use `VGAM::predictvglm()` to avoid mis-dispatch
  # when 'VGAM' is not attached and `predict()` calls `stats::predict()`
  orig_pred <- VGAM::predictvglm(
    tidy_fit$fit,
    newdata = house_sub,
    type = "response"
  )
  colnames(orig_pred) <- paste0(".pred_", tidy_fit$lvl)
  orig_pred <- tibble::as_tibble(orig_pred)

  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: linear predictor -------------------------------------------------

test_that("linear_pred prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "vglm", parallel_reg = TRUE) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_link <- VGAM::predictvglm(
    tidy_fit$fit,
    newdata = house_sub,
    type = "link"
  )
  orig_pred <- coef(tidy_fit$fit)[1] - orig_link[, 1]
  orig_pred <- tibble::tibble(.pred_linear_pred = unname(orig_pred))
  tidy_pred <- predict(tidy_fit, house_sub, type = "linear_pred")
  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("QSARdata")

  onet_spec <-
    ordinal_reg(parallel_reg = TRUE) |>
    set_mode("classification") |>
    set_engine("vglm")
  expect_snapshot(onet_spec |> translate())

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
      parallel_reg = TRUE,
      ordinal_link = "cloglog", odds_link = "stopping_ratio"
    ) |>
    set_mode("classification") |>
    set_engine("vglm")
  expect_snapshot(onet_arg_spec |> translate())

  expect_snapshot({
    set.seed(13)
    onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(onet_arg_fit$fit@family@infos()$link, "clogloglink")
  expect_equal(onet_arg_fit$fit@family@infos()$parallel, TRUE)
  expect_equal(onet_arg_fit$fit@family@vfamily[1L], "sratio")
})

# parallel regression ----------------------------------------------------------

test_that("parallel regression argument handles logicals", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  house_sub <- get_house()$sub

  # all parallel regression

  set.seed(seed)
  tidy_fit <- ordinal_reg(parallel_reg = TRUE, engine = "vglm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- VGAM::vglm(
    Sat ~ Infl + Cont,
    family = VGAM::cumulative(parallel = TRUE),
    data = house_sub
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(tidy_fit$fit), skip_slots)) {
    expect_equal(
      slot(tidy_fit$fit, s),
      slot(orig_fit, s),
      ignore_attr = TRUE, ignore_formula_env = TRUE
    )
  }

  # all category-specific

  set.seed(seed)
  tidy_fit <- ordinal_reg(parallel_reg = FALSE, engine = "vglm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- VGAM::vglm(
    Sat ~ Infl + Cont,
    family = VGAM::cumulative(parallel = FALSE),
    data = house_sub
  )

  skip_slots <- c("call", "misc")
  for (s in setdiff(slotNames(tidy_fit$fit), skip_slots)) {
    expect_equal(
      slot(tidy_fit$fit, s),
      slot(orig_fit, s),
      ignore_attr = TRUE, ignore_formula_env = TRUE
    )
  }
})

# argument translation ---------------------------------------------------------

test_that("standardized link, family, and threshold values are matched", {
  expect_equal(match_ordinal_link_VGAM("logistic"), "logitlink")
  expect_equal(match_ordinal_link_VGAM("cloglog"), "clogloglink")
  expect_equal(match_ordinal_link_VGAM("probit"), "probitlink")
  expect_equal(match_ordinal_link_VGAM("foldsqrtlink"), "foldsqrtlink")
  expect_equal(match_ordinal_family("cumulative_link"), "cumulative")
  expect_equal(match_ordinal_family("stopping_ratio"), "sratio")
  expect_equal(match_ordinal_family("sratio"), "sratio")
  expect_equal(match_threshold_structure_VGAM("equidistant"), "equid")
  expect_equal(match_threshold_structure_VGAM("symmetric_zero"), "symm0")
  expect_equal(match_threshold_structure_VGAM("qnorm"), "qnorm")

  expect_snapshot(error = TRUE, {
    match_ordinal_link_VGAM("loglog")
  })
  expect_snapshot(error = TRUE, {
    match_ordinal_link_VGAM("logisitc")
  })
  expect_snapshot(error = TRUE, {
    match_ordinal_family("cumu")
  })
  expect_snapshot(error = TRUE, {
    match_threshold_structure_VGAM(c("flexible", "equidistant"))
  })
})

test_that("the adjacent categories family rejects incompatible links", {
  expect_no_error(
    check_ordinal_link_family_VGAM(family = "acat", link = "cauchitlink")
  )
  expect_snapshot(error = TRUE, {
    check_ordinal_link_family_VGAM(family = "acat", link = "logitlink")
  })
})

test_that("VGAM wrappers translate standardized argument values", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  house_data <-
    MASS::housing[rep(seq(nrow(MASS::housing)), MASS::housing$Freq), -5]

  # native values pass through unchanged
  native <- VGAM_vglm_wrapper(
    Sat ~ Infl + Type, data = house_data,
    family = "sratio", link = "probitlink", Thresh = "symm1", parallel = TRUE
  )
  expect_equal(native@family@infos()$link, "probitlink")
  expect_equal(native@family@vfamily[1L], "sratio")

  # standardized values are converted
  standardized <- VGAM_vglm_wrapper(
    Sat ~ Infl + Type, data = house_data,
    family = "stopping_ratio", link = "probit", parallel = TRUE,
    Thresh = "symmetric_median"
  )
  expect_equal(standardized@family@infos()$link, "probitlink")
  expect_equal(standardized@family@infos()$parallel, TRUE)
  expect_equal(standardized@family@vfamily[1L], "sratio")
})
