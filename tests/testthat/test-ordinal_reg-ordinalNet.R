seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  # https://stackoverflow.com/a/4569239
  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL
  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_sub$Sat,
    lambdaVals = .001
    # , alpha = .5, family = "sratio"
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("ordinalNet") |>
    set_mode("classification") |>
    set_args(penalty = .001)
  # tidy_spec <- set_args(tidy_spec, mixture = .5, odds_link = "stopping")
  set.seed(seed)
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

# TODO: Once fitting with weights is enabled, write a test.

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = .001) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL

  # FIXME: Be able to specify new data.
  # Error in cbind(1, x) %*% betaMat : non-conformable arguments
  # orig_pred <- predict(tidy_fit$fit, newx = house_vars, type = "class")
  orig_pred <- predict(tidy_fit$fit, type = "class")
  orig_pred <- ordered(tidy_fit$lvl[orig_pred], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, house_sub)
  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = .001) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL

  # FIXME: Be able to specify new data.
  # orig_pred <- predict(tidy_fit$fit, newx = house_vars, type = "response")
  orig_pred <- predict(tidy_fit$fit, type = "response")
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", tidy_fit$lvl))
  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")
  expect_equal(orig_pred, tidy_pred)
})
