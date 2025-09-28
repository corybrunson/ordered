seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
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
    # NB: The earlier line may be right if we implement `path_values`.
    # lambdaVals = .001
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
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

# model: multinomial input -----------------------------------------------------------

# REVIEW: This test is necessarily approximate--the `ordinalNet()` interfaces
# don't perfectly agree with each other--but it seems appropriate to include it
# ahead of the case weights test.
# NB: This test fails when the additional (commented) arguments are passed.
test_that("multinomial formulation", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  house_sub |>
    transform(n = 1L) |>
    aggregate(x = n ~ ., data = _, FUN = length) |>
    reshape(
      direction = "wide",
      idvar = c("Infl", "Type", "Cont"),
      timevar = "Sat"
    ) ->
    house_agg

  house_vars <- house_agg[, 1:3]
  house_vars <- model.matrix(
    ~ Type + Infl + Cont + 0, data = house_vars,
    contrasts.arg = lapply(house_vars, contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL

  house_resp <- house_agg[, 4:6]
  house_resp <- as.matrix(house_resp)
  house_resp[is.na(house_resp)] <- 0L
  colnames(house_resp) <- levels(house_sub$Sat)

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_resp,
    # lambdaVals = .001
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
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
  # omit coefficients and iterations
  orig_fit$coefs <- tidy_fit$fit$coefs <- NULL
  orig_fit$iterIn <- tidy_fit$fit$iterIn <- NULL
  orig_fit$args$x <- tidy_fit$fit$args$x <- NULL
  orig_fit$args$y <- tidy_fit$fit$args$y <- NULL

  expect_equal(
    orig_fit,
    tidy_fit$fit,
    ignore_formula_env = TRUE,
    tolerance = .001
  )
})

# model: case weights ----------------------------------------------------------

# NB: This test passes when the additional (commented) arguments are passed.
test_that("case weights", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_data <- get_house()$data

  house_data |>
    transform(
      Low = ifelse(Sat == "Low", Freq, 0L),
      Medium = ifelse(Sat == "Medium", Freq, 0L),
      High = ifelse(Sat == "High", Freq, 0L)
    ) |>
    subset(select = -c(Sat, Freq)) ->
    house_nums

  house_vars <- house_nums[, 1:3]
  house_vars <- model.matrix(
    ~ Type + Infl + Cont + 0, data = house_vars,
    contrasts.arg = lapply(house_vars, contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL

  house_resp <- house_nums[, 4:6]
  house_resp <- as.matrix(house_resp)
  house_resp[is.na(house_resp)] <- 0L
  rownames(house_resp) <- NULL

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_resp,
    # lambdaVals = .001
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
    # , alpha = .5, family = "sratio"
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("ordinalNet") |>
    set_mode("classification") |>
    set_args(penalty = .001)
  # tidy_spec <- set_args(tidy_spec, mixture = .5, odds_link = "stopping")
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = tidy_data,
    case_weights = tidy_data$Freq
  )

  # remove `call` from comparison
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
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  stop("Prediction now uses a wrapper.")

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = .001) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  # NB: `newx` must contain exactly those predictors used in the fit.
  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL
  orig_pred <- predict(tidy_fit$fit, newx = house_vars, type = "class")
  orig_pred <- ordered(tidy_fit$lvl[orig_pred], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)

  tidy_pred <- predict(tidy_fit, house_sub)

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  stop("Prediction now uses a wrapper.")

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = .001) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  # NB: `newx` must contain exactly those predictors used in the fit.
  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )
  attr(house_vars, "assign") <- NULL
  attr(house_vars, "contrasts") <- NULL
  orig_pred <- predict(tidy_fit$fit, newx = house_vars, type = "response")
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", tidy_fit$lvl))

  tidy_pred <- predict(tidy_fit, house_sub, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ordinalNet")
  skip_if_not_installed("QSARdata")

  onet_spec <-
    ordinal_reg(penalty = .001) %>%
    set_mode("classification") %>%
    set_engine("ordinalNet")
  expect_snapshot(onet_spec %>% translate())

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

  rownames(onet_f_fit$fit$args$x) <- NULL
  expect_equal(
    onet_f_fit$fit,
    onet_xy_fit$fit
  )
})

test_that("arguments agree", {
  skip_if_not_installed("ordinalNet")
  skip_if_not_installed("QSARdata")

  onet_arg_spec <-
    ordinal_reg(
      penalty = .001, mixture = .25,
      ordinal_link = "cloglog", odds_link = "stopping"
    ) |>
    set_mode("classification") %>%
    set_engine("ordinalNet")
  expect_snapshot(onet_arg_spec %>% translate())

  expect_snapshot({
    set.seed(13)
    onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(onet_arg_fit$fit$args$lambdaVals, .001)
  expect_equal(onet_arg_fit$fit$args$alpha, .25)
  expect_equal(onet_arg_fit$fit$args$link, "cloglog")
  expect_equal(onet_arg_fit$fit$args$family, "sratio")
})
