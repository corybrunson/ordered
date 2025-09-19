seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalForest")
  house_sub <- get_house()$sub

  # NB: Reconcile different defaults between {parsnip} and {ordinalForest}.
  set.seed(seed)
  orig_fit <- ordinalForest::ordfor(
    depvar = "Sat",
    data = house_sub,
    nsets = 10,
    min.node.size = 20,
    ntreeperdiv = 100,
    ntreefinal = 10
  )

  tidy_spec <- rand_forest() |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(num_scores = 10, min_n = 20, num_score_trees = 100, trees = 10)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Infl + Type + Cont, data = house_sub)

  expect_equal(
    orig_fit,
    tidy_fit$fit
  )
})

test_that("model object w/ probability metric", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalForest")
  house_sub <- get_house()$sub

  set.seed(seed)
  suppressWarnings(
    orig_fit <- ordinalForest::ordfor(
      depvar = "Sat",
      data = house_sub,
      nsets = 10,
      min.node.size = 20,
      ntreeperdiv = 100,
      ntreefinal = 10,
      perffunction = "probability"
    )
  )

  tidy_spec <- rand_forest() |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(num_scores = 10, min_n = 20, num_score_trees = 100, trees = 10,
             ord_metric = "probability")
  set.seed(seed)
  suppressWarnings(
    tidy_fit <- fit(tidy_spec, Sat ~ Infl + Type + Cont, data = house_sub)
  )

  expect_equal(
    orig_fit,
    tidy_fit$fit
  )
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalForest")
  house_sub <- get_house()$sub

  tidy_fit <- rand_forest() |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(num_scores = 10, num_score_trees = 100, trees = 10) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub)
  orig_pred <- ordered(orig_pred$ypred, levels(orig_pred$ypred))
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, ".pred_class")
  tidy_pred <- predict(tidy_fit, house_sub, type = "class")
  expect_equal(orig_pred, tidy_pred)

  expect_error(predict(tidy_fit, house_sub, type = "prob"), "ord_metric")
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalForest")
  house_sub <- get_house()$sub

  tidy_fit <- rand_forest() |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(ord_metric = "probability") |>
    set_args(num_scores = 10, num_score_trees = 100, trees = 10) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub)
  colnames(orig_pred$classprobs) <- paste0(".pred_", levels(orig_pred$ypred))
  orig_pred <- tibble::as_tibble(orig_pred$classprobs)
  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")
  expect_equal(orig_pred, tidy_pred)
})

test_that("interfaces agree",{
  skip_if_not_installed("ordinalForest")
  skip_if_not_installed("QSARdata")

  orf_spec <-
    rand_forest() %>%
    set_mode("classification") %>%
    set_engine("ordinalForest") %>%
    set_args(ord_metric = "probability")

  expect_snapshot(orf_spec %>% translate())

  expect_no_error({
    set.seed(13)
    orf_f_fit <- fit(orf_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(orf_f_fit)

  # orf_f_cls <- predict(orf_f_fit, new_data = caco_test)
  # expect_named(orf_f_cls, ".pred_class")
  # expect_true(nrow(orf_f_cls) == nrow(caco_test))
  # expect_s3_class(orf_f_cls$.pred_class, "ordered")
  #
  # orf_f_prob <- predict(orf_f_fit, new_data = caco_test, type = "prob")
  # expect_named(orf_f_prob, c(".pred_L", ".pred_M", ".pred_H"))
  # expect_true(nrow(orf_f_prob) == nrow(orf_f_prob))

  expect_no_error({
    set.seed(13)
    orf_xy_fit <- fit_xy(orf_spec, x = caco_train[, -1], y = caco_train$class)
  })
  expect_snapshot(orf_xy_fit)

  expect_equal(
    orf_f_fit$fit$perffunctionvalues,
    orf_xy_fit$fit$perffunctionvalues
  )
})

test_that("translation agrees",{
  skip_if_not_installed("ordinalForest")
  skip_if_not_installed("QSARdata")

  orf_arg_spec <-
    rand_forest(mtry = 2, min_n = 11, trees = 100) %>%
    set_mode("classification") %>%
    set_engine("ordinalForest")

  expect_snapshot(orf_arg_spec %>% translate())

  # This warning is a bug that I'll report
  expect_snapshot({
    set.seed(13)
    orf_arg_fit <- fit(orf_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(orf_arg_fit$fit$ntreefinal, 100)
  expect_equal(orf_arg_fit$fit$forestfinal$min.node.size, 11)
  expect_equal(orf_arg_fit$fit$forestfinal$mtry, 2)
})
