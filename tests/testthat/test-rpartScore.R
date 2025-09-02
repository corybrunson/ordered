seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rpartScore")
  house_sub <- get_house()$sub

  set.seed(seed)
  orig_fit <- rpartScore::rpartScore(
    Sat ~ Type + Infl + Cont,
    data = mutate(house_sub, Sat = as.integer(Sat))
  )

  tidy_spec <- decision_tree() |>
    set_engine("rpartScore") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  # remove `call` from comparison
  orig_fit$call <- NULL
  tidy_fit$fit$call <- NULL

  expect_equal(
    orig_fit,
    tidy_fit$fit,
    ignore_formula_env = TRUE,
    ignore_function_env = TRUE
  )
})

# model: case weights ----------------------------------------------------------

test_that("case weights", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rpartScore")
  house_data <- get_house()$data

  set.seed(seed)
  orig_fit <- rpartScore::rpartScore(
    Sat ~ Type + Infl + Cont,
    data = mutate(house_data, Sat = as.integer(Sat)),
    weights = Freq
  )

  tidy_spec <- decision_tree() |>
    set_engine("rpartScore") |>
    set_mode("classification")
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  set.seed(seed)
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
    ignore_formula_env = TRUE,
    ignore_function_env = TRUE
  )
})

# prediction: class ------------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rpartScore")
  house_sub <- get_house()$sub

  tidy_fit <- decision_tree() |>
    set_engine("rpartScore") |>
    set_mode("classification") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub, type = "vector")
  orig_pred <- ordered(levels(house_sub$Sat)[orig_pred], levels(house_sub$Sat))
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, ".pred_class")
  tidy_pred <- predict(tidy_fit, house_sub, type = "class")
  expect_equal(orig_pred, tidy_pred)
})
