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

test_that("class prediction", {
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

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("rpartScore")
  skip_if_not_installed("QSARdata")

  odt_spec <-
    decision_tree() %>%
    set_mode("classification") %>%
    set_engine("rpartScore")
  expect_snapshot(odt_spec %>% translate())

  expect_no_error({
    set.seed(13)
    odt_f_fit <- fit(odt_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(odt_f_fit)

  expect_no_error({
    set.seed(13)
    odt_xy_fit <- fit_xy(odt_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(odt_xy_fit)

  expect_equal(
    odt_f_fit$fit$frame,
    odt_xy_fit$fit$frame
  )
  expect_equal(
    odt_f_fit$fit$splits,
    odt_xy_fit$fit$splits
  )
})

test_that("arguments agree", {
  skip_if_not_installed("rpartScore")
  skip_if_not_installed("QSARdata")

  odt_def_spec <-
    decision_tree() |>
    set_mode("classification") %>%
    set_engine("rpartScore")
  expect_snapshot(odt_def_spec %>% translate())

  odt_arg_spec <-
    decision_tree(
      cost_complexity = .01, tree_depth = 3, min_n = 7,
      split_func = "quad", prune_func = "mr"
    ) |>
    set_mode("classification") %>%
    set_engine("rpartScore")
  expect_snapshot(odt_arg_spec %>% translate())

  expect_snapshot({
    set.seed(13)
    odt_def_fit <- fit(odt_def_spec, class ~ ., data = caco_train)
  })
  expect_snapshot({
    set.seed(13)
    odt_arg_fit <- fit(odt_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(odt_arg_fit$fit$control$cp, .01)
  expect_equal(odt_arg_fit$fit$control$maxdepth, 3)
  expect_equal(odt_arg_fit$fit$control$minsplit, 7)
  # `split` and `prune` specify `split` and `eval` functions.
  odt_def_funs <- odt_def_fit$fit$functions
  odt_arg_funs <- odt_arg_fit$fit$functions
  expect_true(identical(odt_def_funs$summary, odt_arg_funs$summary,
                        ignore.environment = TRUE))
  expect_true(identical(odt_def_funs$text, odt_arg_funs$text,
                        ignore.environment = TRUE))
  expect_false(identical(odt_def_funs$eval, odt_arg_funs$eval,
                         ignore.environment = TRUE))
  expect_false(identical(odt_def_funs$split, odt_arg_funs$split,
                         ignore.environment = TRUE))
  expect_true(identical(odt_def_funs$init, odt_arg_funs$init,
                        ignore.environment = TRUE))
})
