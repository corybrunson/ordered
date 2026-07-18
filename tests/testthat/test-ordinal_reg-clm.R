seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  # no extra arguments

  set.seed(seed)
  orig_fit <- ordinal::clm(Sat ~ Type + Infl + Cont, data = house_sub)

  tidy_spec <- ordinal_reg() |>
    set_engine("clm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL
  orig_fit$formulas <- tidy_fit$fit$formulas <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_formula_env = TRUE)

  # extra arguments

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Type + Infl + Cont,
    data = house_sub,
    link = "probit",
    threshold = "symmetric"
  )

  tidy_spec <- ordinal_reg(
    ordinal_link = "probit", threshold_structure = "symmetric_median"
  ) |>
    set_engine("clm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL
  orig_fit$formulas <- tidy_fit$fit$formulas <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_formula_env = TRUE)
})

# model: case weights ----------------------------------------------------------

test_that("case weights", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_data <- get_house()$data

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Type + Infl + Cont,
    data = house_data,
    weights = Freq
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("clm") |>
    set_mode("classification")
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = tidy_data,
    case_weights = tidy_data$Freq
  )

  orig_fit$call <- tidy_fit$fit$call <- NULL
  orig_fit$formulas <- tidy_fit$fit$formulas <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_formula_env = TRUE)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "clm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, house_sub, type = "class")$fit
  orig_pred <- tibble::tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, house_sub)
  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "clm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(
    tidy_fit$fit,
    newdata = house_sub[, !names(house_sub) %in% "Sat"],
    type = "prob"
  )$fit
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", names(orig_pred)))
  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")
  expect_equal(orig_pred, tidy_pred)
})

# prediction: linear_pred ------------------------------------------------------

test_that("linear_pred prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "clm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_link <- predict(
    tidy_fit$fit,
    newdata = house_sub[, !names(house_sub) %in% "Sat"],
    type = "linear.predictor"
  )
  orig_pred <- tidy_fit$fit$alpha[1] - orig_link$eta1[, 1]
  orig_pred <- tibble::tibble(.pred_linear_pred = unname(orig_pred))
  tidy_pred <- predict(tidy_fit, house_sub, type = "linear_pred")
  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("QSARdata")

  clm_spec <-
    ordinal_reg() |>
    set_mode("classification") |>
    set_engine("clm")
  expect_snapshot(clm_spec |> translate())

  set.seed(seed)
  expect_no_error({
    clm_f_fit <- suppressWarnings(
      fit(clm_spec, class ~ ., data = caco_train)
    )
  })
  expect_snapshot(clm_f_fit)

  set.seed(seed)
  expect_no_error({
    clm_xy_fit <- suppressWarnings(
      fit_xy(clm_spec, x = caco_train[, -1], y = caco_train$class)
    )
  })
  expect_snapshot(clm_xy_fit)

  clm_f_fit$fit$call <- clm_xy_fit$fit$call <- NULL
  clm_f_fit$fit$formula <- clm_xy_fit$fit$formula <- NULL
  clm_f_fit$fit$formulas <- clm_xy_fit$fit$formulas <- NULL
  clm_f_fit$fit$terms <- clm_xy_fit$fit$terms <- NULL
  clm_f_fit$fit$y <- clm_xy_fit$fit$y <- NULL
  expect_equal(clm_f_fit$fit, clm_xy_fit$fit, ignore_attr = TRUE)
})

test_that("arguments agree", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("QSARdata")

  clm_arg_spec <-
    ordinal_reg(ordinal_link = "probit") |>
    set_mode("classification") |>
    set_engine("clm")
  expect_snapshot(clm_arg_spec |> translate())

  set.seed(seed)
  expect_no_error({
    clm_arg_fit <- suppressWarnings(
      fit(clm_arg_spec, class ~ ., data = caco_train)
    )
  })
  expect_equal(clm_arg_fit$fit$link, "probit")
})

# parallel regression ----------------------------------------------------------

test_that("parallel regression argument handles logicals", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  # all parallel regression

  set.seed(seed)
  tidy_fit <- ordinal_reg(parallel_reg = TRUE, engine = "clm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Infl + Cont, data = house_sub
  )

  tidy_fit$fit$call <- orig_fit$call <- NULL
  tidy_fit$fit$formulas <- orig_fit$formulas <- NULL
  expect_equal(tidy_fit$fit, orig_fit, ignore_formula_env = TRUE)

  # all category-specific

  set.seed(seed)
  tidy_fit <- ordinal_reg(parallel_reg = FALSE, engine = "clm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Infl + Cont, data = house_sub,
    nominal = ~ Infl + Cont
  )

  tidy_fit$fit$call <- orig_fit$call <- NULL
  tidy_fit$fit$formulas <- orig_fit$formulas <- NULL
  expect_equal(tidy_fit$fit, orig_fit, ignore_formula_env = TRUE)
})

test_that("parallel regression argument handles formulae", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  set.seed(seed)
  tidy_fit1 <- ordinal_reg(parallel_reg = FALSE ~ Cont, engine = "clm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  tidy_fit2 <- ordinal_reg(parallel_reg = TRUE ~ Infl, engine = "clm") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Infl + Cont, data = house_sub,
    nominal = ~ Cont
  )

  tidy_fit1$fit$call <- tidy_fit2$fit$call <- orig_fit$call <- NULL
  tidy_fit1$fit$formulas <- tidy_fit2$fit$formulas <- orig_fit$formulas <- NULL
  expect_equal(tidy_fit1$fit, orig_fit, ignore_formula_env = TRUE)
  expect_equal(tidy_fit2$fit, orig_fit, ignore_formula_env = TRUE)
})

test_that("parallel regression argument handles lists", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  house_sub <- get_house()$sub

  set.seed(seed)
  tidy_fit <- ordinal_reg(
    parallel_reg = list(TRUE ~ Infl, FALSE ~ Cont),
    engine = "clm"
  ) |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinal::clm(
    Sat ~ Infl + Cont, data = house_sub,
    nominal = ~ Cont
  )

  tidy_fit$fit$call <- orig_fit$call <- NULL
  tidy_fit$fit$formulas <- orig_fit$formulas <- NULL
  expect_equal(tidy_fit$fit, orig_fit, ignore_formula_env = TRUE)

  expect_snapshot(
    ordinal_reg(
      parallel_reg = list(FALSE ~ Infl, TRUE ~ Infl + Cont),
      engine = "clm"
    ) |>
      fit(Sat ~ Infl + Cont, data = house_sub),
    error = TRUE
  )
})
