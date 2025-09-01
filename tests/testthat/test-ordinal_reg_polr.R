
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
