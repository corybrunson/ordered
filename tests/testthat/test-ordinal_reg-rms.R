seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object (lrm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  # no extra arguments

  set.seed(seed)
  orig_fit <- rms::lrm(
    Sat ~ Type + Infl + Cont,
    data = house_sub
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("lrm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)

  # extra arguments

  set.seed(seed)
  orig_fit <- rms::lrm(
    Sat ~ Type + Infl + Cont,
    data = house_sub,
    penalty = .01
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("lrm") |>
    set_mode("classification") |>
    set_args(penalty = .01)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)
})

test_that("model object (orm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  # no extra arguments

  set.seed(seed)
  orig_fit <- rms::orm(
    Sat ~ Type + Infl + Cont,
    data = house_sub
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("orm") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)

  # extra arguments

  set.seed(seed)
  orig_fit <- rms::orm(
    Sat ~ Type + Infl + Cont,
    data = house_sub,
    family = "probit",
    penalty = .01
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("orm") |>
    set_mode("classification") |>
    set_args(ordinal_link = "probit", penalty = .01)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)
})

# model: case weights ----------------------------------------------------------

test_that("case weights (lrm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  set.seed(seed)
  house_wts <- rpois(n = nrow(house_sub), 2) + 1L

  set.seed(seed)
  expect_warning(
    orig_fit <- rms::lrm(
      Sat ~ Type + Infl + Cont,
      data = house_sub,
      weights = house_wts
    ),
    regexp = "weights"
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("lrm") |>
    set_mode("classification")
  set.seed(seed)
  expect_warning(
    tidy_fit <- fit(
      tidy_spec,
      Sat ~ Type + Infl + Cont,
      data = house_sub,
      case_weights = frequency_weights(house_wts)
    ),
    regexp = "weights"
  )

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)
})

test_that("case weights (orm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  set.seed(seed)
  house_wts <- rpois(n = nrow(house_sub), 2) + 1L

  set.seed(seed)
  expect_warning(
    orig_fit <- rms::orm(
      Sat ~ Type + Infl + Cont,
      data = house_sub,
      weights = house_wts
    ),
    regexp = "weights"
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("orm") |>
    set_mode("classification")
  set.seed(seed)
  expect_warning(
    tidy_fit <- fit(
      tidy_spec,
      Sat ~ Type + Infl + Cont,
      data = house_sub,
      case_weights = frequency_weights(house_wts)
    ),
    regexp = "weights"
  )

  orig_fit$call <- tidy_fit$fit$call <- NULL

  expect_equal(coef(orig_fit), coef(tidy_fit$fit))
  expect_equal(orig_fit$freq, tidy_fit$fit$freq)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction (lrm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "lrm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_prob <- predict(tidy_fit$fit, newdata = house_sub, type = "fitted.ind")
  orig_class <- apply(orig_prob, 1L, which.max)
  orig_class <- ordered(tidy_fit$lvl[orig_class], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_class)

  tidy_pred <- predict(tidy_fit, house_sub)

  expect_equal(orig_pred, tidy_pred)
})

test_that("class prediction (orm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "orm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_prob <- predict(tidy_fit$fit, newdata = house_sub, type = "fitted.ind")
  orig_class <- apply(orig_prob, 1L, which.max)
  orig_class <- ordered(tidy_fit$lvl[orig_class], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_class)

  tidy_pred <- predict(tidy_fit, house_sub)

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction (lrm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "lrm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_prob <- predict(tidy_fit$fit, newdata = house_sub, type = "fitted.ind")
  orig_pred <- tibble::as_tibble(orig_prob)
  orig_pred <- set_names(orig_pred, paste0(".pred_", tidy_fit$lvl))

  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

test_that("probability prediction (orm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "orm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_prob <- predict(tidy_fit$fit, newdata = house_sub, type = "fitted.ind")
  orig_pred <- tibble::as_tibble(orig_prob)
  orig_pred <- set_names(orig_pred, paste0(".pred_", tidy_fit$lvl))

  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: tibble input -----------------------------------------------------

test_that("tibble input (lrm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "lrm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  tib_sub <- tibble::as_tibble(house_sub)

  expect_no_error(predict(tidy_fit, tib_sub))
  expect_no_error(predict(tidy_fit, tib_sub, type = "prob"))
})

test_that("tibble input (orm)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "orm") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  tib_sub <- tibble::as_tibble(house_sub)

  expect_no_error(predict(tidy_fit, tib_sub))
  expect_no_error(predict(tidy_fit, tib_sub, type = "prob"))
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree (lrm)", {
  skip_if_not_installed("rms")
  skip_if_not_installed("QSARdata")

  lrm_spec <-
    ordinal_reg() |>
    set_mode("classification") |>
    set_engine("lrm")
  expect_snapshot(lrm_spec |> translate())

  expect_no_error({
    set.seed(13)
    lrm_f_fit <- fit(lrm_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(lrm_f_fit)

  expect_no_error({
    set.seed(13)
    lrm_xy_fit <- fit_xy(lrm_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(lrm_xy_fit)

  lrm_f_fit$fit$call <- lrm_xy_fit$fit$call <- NULL

  expect_equal(
    coef(lrm_f_fit$fit),
    coef(lrm_xy_fit$fit),
    ignore_attr = TRUE
  )
})

test_that("interfaces agree (orm)", {
  skip_if_not_installed("rms")
  skip_if_not_installed("QSARdata")

  orm_spec <-
    ordinal_reg() |>
    set_mode("classification") |>
    set_engine("orm")
  expect_snapshot(orm_spec |> translate())

  expect_no_error({
    set.seed(13)
    orm_f_fit <- fit(orm_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(orm_f_fit)

  expect_no_error({
    set.seed(13)
    orm_xy_fit <- fit_xy(orm_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(orm_xy_fit)

  orm_f_fit$fit$call <- orm_xy_fit$fit$call <- NULL

  expect_equal(
    coef(orm_f_fit$fit),
    coef(orm_xy_fit$fit),
    ignore_attr = TRUE
  )
})

test_that("arguments agree (lrm)", {
  skip_if_not_installed("rms")
  skip_if_not_installed("QSARdata")

  lrm_arg_spec <-
    ordinal_reg(penalty = 0.1) |>
    set_mode("classification") |>
    set_engine("lrm")
  expect_snapshot(lrm_arg_spec |> translate())

  expect_snapshot({
    set.seed(13)
    lrm_arg_fit <- fit(lrm_arg_spec, class ~ ., data = caco_train)
  })

  # The penalty is passed to lrm(); compare to a direct call
  expect_equal(
    lrm_arg_fit$fit$penalty,
    list(simple = 0.1, nonlinear = 0.1,
         interaction = 0.1, nonlinear.interaction = 0.1)
  )
})

test_that("arguments agree (orm)", {
  skip_if_not_installed("rms")
  skip_if_not_installed("QSARdata")

  orm_arg_spec <-
    ordinal_reg(ordinal_link = "cauchit", penalty = 0.1) |>
    set_mode("classification") |>
    set_engine("orm")
  expect_snapshot(orm_arg_spec |> translate())

  expect_snapshot({
    set.seed(13)
    orm_arg_fit <- fit(orm_arg_spec, class ~ ., data = caco_train)
  })

  expect_equal(orm_arg_fit$fit$family, "cauchit")
  expect_equal(
    orm_arg_fit$fit$penalty,
    list(simple = 0.1, nonlinear = 0.1,
         interaction = 0.1, nonlinear.interaction = 0.1)
  )
})
