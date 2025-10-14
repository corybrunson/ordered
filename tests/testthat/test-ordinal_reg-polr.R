
# specification: arguments -----------------------------------------------------

test_that("ordinal_link", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  # a legitimate ordinal link function not recognized by {dials}
  tidy_spec <- ordinal_reg(engine = "polr", ordinal_link = "Aranda-Ordaz")
  expect_error(fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub))
})

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  orig_fit <- MASS::polr(
    Sat ~ Type + Infl + Cont,
    data = house_sub,
    model = TRUE
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("polr") |>
    set_mode("classification")
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  # remove `call` from comparison
  orig_fit$call <- NULL
  tidy_fit$fit$call <- NULL

  expect_equal(
    orig_fit,
    tidy_fit$fit,
    ignore_formula_env = TRUE
  )
})

# model: case weights ----------------------------------------------------------

test_that("case weights", {
  skip_if_not_installed("MASS")
  house_data <- get_house()$data

  orig_fit <- MASS::polr(
    Sat ~ Type + Infl + Cont,
    data = house_data,
    weights = Freq,
    model = TRUE
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("polr") |>
    set_mode("classification")
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = tidy_data,
    case_weights = tidy_data$Freq
  )

  orig_fit$call <- NULL
  tidy_fit$fit$call <- NULL

  expect_equal(
    orig_fit,
    tidy_fit$fit,
    ignore_formula_env = TRUE
  )
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg() |>
    set_engine("polr") |>
    fit(Sat ~ Infl + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, house_sub)
  # NB: `MASS:::predict.polr()` strips order from `object$model$<outcome>`.
  orig_pred <- ordered(unname(orig_pred), levels(orig_pred))
  orig_pred <- tibble::tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, house_sub)
  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg() |>
    set_engine("polr") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub, type = "probs")
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", names(orig_pred)))
  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")
  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("QSARdata")

  olpr_spec <-
    ordinal_reg() %>%
    set_mode("classification") %>%
    set_engine("polr")
  expect_snapshot(olpr_spec %>% translate())

  expect_no_error({
    set.seed(13)
    olpr_f_fit <- fit(olpr_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(olpr_f_fit)

  expect_no_error({
    set.seed(13)
    olpr_xy_fit <- fit_xy(olpr_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(olpr_xy_fit)

  olpr_f_fit$fit$terms <- NULL
  olpr_xy_fit$fit$terms <- NULL
  names(olpr_xy_fit$fit$model)[1] <- "class"
  olpr_f_fit$fit$call <- NULL
  olpr_xy_fit$fit$call <- NULL
  expect_equal(
    olpr_f_fit$fit,
    olpr_xy_fit$fit,
    ignore_attr = TRUE
  )
})

test_that("arguments agree", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("QSARdata")

  olpr_arg_spec <-
    ordinal_reg(ordinal_link = "cloglog") |>
    set_mode("classification") %>%
    set_engine("polr")
  expect_snapshot(olpr_arg_spec %>% translate())

  expect_snapshot({
    set.seed(13)
    olpr_arg_fit <- fit(olpr_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(olpr_arg_fit$fit$method, "cloglog")
})
