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
    set_args(
      nsets = 10, min_n = 20, ntreeperdiv = 100, trees = 10,
      # prevent 'min.node.size' warning
      perffunction = "equal"
    )
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
    set_args(
      nsets = 10, min_n = 20, ntreeperdiv = 100, trees = 10,
      perffunction = "probability"
    )
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

  # with class probabilities (default)
  tidy_fit <- rand_forest(trees = 10) |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(nsets = 10, ntreeperdiv = 100) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub)
  orig_pred <- ordered(orig_pred$ypred, levels(orig_pred$ypred))
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, ".pred_class")
  tidy_pred <- predict(tidy_fit, house_sub, type = "class")
  expect_equal(orig_pred, tidy_pred)

  # without class probabilities
  tidy_fit <- rand_forest(trees = 10) |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(nsets = 10, ntreeperdiv = 100, perffunction = "equal") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub)
  orig_pred <- ordered(orig_pred$ypred, levels(orig_pred$ypred))
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, ".pred_class")
  tidy_pred <- predict(tidy_fit, house_sub, type = "class")
  # FIXME: Predicted classes do not agree.
  # expect_equal(orig_pred, tidy_pred)

  expect_error(predict(tidy_fit, house_sub, type = "prob"), "perffunction")
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalForest")
  house_sub <- get_house()$sub

  tidy_fit <- rand_forest() |>
    set_engine("ordinalForest") |>
    set_mode("classification") |>
    set_args(perffunction = "probability") |>
    set_args(nsets = 10, ntreeperdiv = 100, trees = 10) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  orig_pred <- predict(tidy_fit$fit, newdata = house_sub)
  colnames(orig_pred$classprobs) <- paste0(".pred_", levels(orig_pred$ypred))
  orig_pred <- tibble::as_tibble(orig_pred$classprobs)
  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")
  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ordinalForest")
  skip_if_not_installed("QSARdata")

  orf_spec <-
    rand_forest() %>%
    set_mode("classification") %>%
    set_engine("ordinalForest") %>%
    set_args(perffunction = "probability")
  expect_snapshot(orf_spec %>% translate())

  expect_no_error({
    set.seed(13)
    orf_f_fit <- fit(orf_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(orf_f_fit)

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

test_that("arguments agree", {
  skip_if_not_installed("ordinalForest")
  skip_if_not_installed("QSARdata")

  orf_arg_spec <-
    rand_forest(mtry = 2, min_n = 11, trees = 100) %>%
    set_mode("classification") %>%
    set_engine(
      "ordinalForest",
      nsets = 50, ntreeperdiv = 80, npermtrial = 70, nbest = 10
    ) %>%
    set_args(perffunction = "probability")
  expect_snapshot(orf_arg_spec %>% translate())

  # This warning is a bug that I'll report
  expect_snapshot({
    set.seed(13)
    orf_arg_fit <- fit(orf_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(orf_arg_fit$fit$nsets, 50)
  expect_equal(orf_arg_fit$fit$ntreeperdiv, 80)
  # NB: `npermtrial` value does not seem to be recoverable from model.
  expect_equal(orf_arg_fit$fit$nbest, 10)
  expect_equal(orf_arg_fit$fit$ntreefinal, 100)
  expect_equal(orf_arg_fit$fit$forestfinal$min.node.size, 11)
  expect_equal(orf_arg_fit$fit$forestfinal$mtry, 2)
  expect_equal(orf_arg_fit$fit$perffunction, "probability")
})
