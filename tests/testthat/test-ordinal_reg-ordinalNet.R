# model: basic -----------------------------------------------------------------

test_that("model object (penalty path from original to tidy)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  # https://stackoverflow.com/a/4569239
  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )

  # no extra arguments

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_sub$Sat,
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
  )

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("ordinalNet", path_values = !!orig_fit$lambdaVals)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  expect_equal(
    orig_fit$coefs,
    tidy_fit$fit$coefs,
    ignore_formula_env = TRUE
  )

  # extra arguments

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_sub$Sat,
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE,
    alpha = .5, family = "sratio"
  )

  tidy_spec <-
    ordinal_reg(penalty = 0.01, mixture = .5, odds_link = "stopping") |>
    set_engine("ordinalNet", path_values = !!orig_fit$lambdaVals)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  expect_equal(
    orig_fit$coefs,
    tidy_fit$fit$coefs,
    ignore_formula_env = TRUE,
    tolerance = 0.001
  )
})

test_that("model object (penalty path from tidy to original)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  # https://stackoverflow.com/a/4569239
  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )

  # no extra arguments

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("ordinalNet") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_sub$Sat,
    lambdaVals = tidy_fit$fit$lambdaVals
  )

  expect_equal(
    orig_fit$coefs,
    tidy_fit$fit$coefs,
    ignore_formula_env = TRUE
  )

  # extra arguments

  tidy_spec <-
    ordinal_reg(penalty = 0.001, mixture = .5, odds_link = "stopping") |>
    set_engine("ordinalNet")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_sub$Sat,
    lambdaVals = tidy_fit$fit$lambdaVals,
    alpha = .5, family = "sratio"
  )

  # FIXME: Why are these not equal? (The penalty paths are equal.)
  orig_fit$args <- tidy_fit$fit$args <- NULL
  expect_equal(
    orig_fit,
    tidy_fit$fit,
    ignore_attr = TRUE
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

  house_resp <- house_nums[, 4:6]
  house_resp <- as.matrix(house_resp)
  house_resp[is.na(house_resp)] <- 0L
  rownames(house_resp) <- NULL

  # no extra arguments

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_resp,
    # lambdaVals = .001
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
  )

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("ordinalNet", path_values = !!orig_fit$lambdaVals)
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = tidy_data,
    case_weights = tidy_data$Freq
  )

  orig_fit$args <- tidy_fit$fit$args <- NULL
  expect_equal(
    orig_fit,
    tidy_fit$fit
  )

  # extra arguments

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_resp,
    # lambdaVals = .001
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE,
    alpha = .5, family = "sratio"
  )

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("ordinalNet", path_values = !!orig_fit$lambdaVals) |>
    set_args(mixture = .5, odds_link = "stopping")
  tidy_data <- transform(house_data, Freq = frequency_weights(Freq))
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont,
    data = tidy_data,
    case_weights = tidy_data$Freq
  )

  orig_fit$args <- tidy_fit$fit$args <- NULL
  expect_equal(
    orig_fit,
    tidy_fit$fit
  )
})

# model: multinomial input -----------------------------------------------------

# REVIEW: This test is necessarily approximate--the `ordinalNet()` interfaces
# don't perfectly agree with each other--but it seems appropriate to include it
# ahead of the case weights test.
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

  house_resp <- house_agg[, 4:6]
  house_resp <- as.matrix(house_resp)
  house_resp[is.na(house_resp)] <- 0L
  colnames(house_resp) <- levels(house_sub$Sat)

  set.seed(seed)
  orig_fit <- ordinalNet::ordinalNet(
    house_vars,
    y = house_resp,
    nLambda = 120, lambdaMinRatio = .001, includeLambda0 = TRUE
  )

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("ordinalNet", path_values = !!orig_fit$lambdaVals)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  # compare summaries
  expect_equal(
    summary(orig_fit) |> as.data.frame() |> subset(select = -nNonzero),
    summary(tidy_fit$fit) |> as.data.frame() |> subset(select = -nNonzero),
    ignore_formula_env = TRUE,
    tolerance = .1
  )
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(penalty = 0.01, engine = "ordinalNet") |>
    fit(Sat ~ Type + Cont, data = house_sub)

  # NB: `newx` must contain exactly those predictors used in the fit.
  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )

  orig_pred <- predict(tidy_fit$fit, newx = house_vars, type = "class")
  orig_pred <- ordered(tidy_fit$lvl[orig_pred], tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)

  tidy_pred <- predict(tidy_fit, house_sub)

  expect_equal(orig_pred, tidy_pred)
})

# multiple prediction ----------------------------------------------------------

test_that("multiple prediction structure", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = 1) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )

  pen_vals <- tidy_fit$fit$lambda[length(tidy_fit$fit$lambda) * seq(4) / 5]

  # class

  multi_pred <- multi_predict(
    tidy_fit,
    new_data = house_sub,
    type = "class",
    penalty = pen_vals
  )

  # class & outer structure
  expect_s3_class(multi_pred, "tbl_df")
  expect_equal(nrow(multi_pred), nrow(house_sub))
  expect_true(".pred" %in% names(multi_pred))
  # one row per penalty value
  nested_rows <- unique(purrr::map_int(multi_pred$.pred, nrow))
  expect_equal(nested_rows, length(pen_vals))
  # one column for prediction + one column for penalty
  nested_cols <- unique(purrr::map_int(multi_pred$.pred, ncol))
  expect_equal(nested_cols, 2L)

  # probability

  multi_pred <- multi_predict(
    tidy_fit,
    new_data = house_sub,
    type = "prob",
    penalty = pen_vals
  )

  # class & outer structure
  expect_s3_class(multi_pred, "tbl_df")
  expect_equal(nrow(multi_pred), nrow(house_sub))
  expect_true(".pred" %in% names(multi_pred))
  # one row per penalty value
  nested_rows <- unique(purrr::map_int(multi_pred$.pred, nrow))
  expect_equal(nested_rows, length(pen_vals))
  # one column per class + one column for penalty
  nested_cols <- unique(purrr::map_int(multi_pred$.pred, ncol))
  expect_equal(nested_cols, length(tidy_fit$lvl) + 1L)
})

test_that("multiple prediction values match sequential prediction values", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(engine = "ordinalNet", penalty = 1) |>
    fit(Sat ~ Type + Cont, data = house_sub)

  pen_vals <- tidy_fit$fit$lambda[length(tidy_fit$fit$lambda) * seq(4) / 5]

  # class

  multi_pred <- tidy_fit |>
    multi_predict(house_sub, type = "class", penalty = pen_vals) |>
    tidyr::unnest(cols = c(.pred))
  for (i in seq_along(pen_vals)) {
    single_pred <- predict(
      tidy_fit, house_sub, type = "class", penalty = pen_vals[i]
    )
    expect_equal(
      multi_pred |>
        filter(penalty == pen_vals[i]) |>
        dplyr::select(-penalty),
      single_pred
    )
  }

  # probability

  multi_pred <- tidy_fit |>
    multi_predict(house_sub, type = "prob", penalty = pen_vals) |>
    tidyr::unnest(cols = c(.pred))
  for (i in seq_along(pen_vals)) {
    single_pred <- predict(
      tidy_fit, house_sub, type = "prob", penalty = pen_vals[i]
    )
    expect_equal(
      multi_pred |>
        filter(penalty == pen_vals[i]) |>
        dplyr::select(-penalty),
      single_pred
    )
  }
})

# prediction: linear_pred ------------------------------------------------------

test_that("linear_pred prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinalNet")
  house_sub <- get_house()$sub

  tidy_fit <- ordinal_reg(penalty = 0.01, engine = "ordinalNet") |>
  fit(Sat ~ Type + Infl + Cont, data = house_sub)

  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )
  orig_link <- predict(tidy_fit$fit, newx = house_vars, type = "link")
  orig_pred <- tidy_fit$fit$coefs[nrow(tidy_fit$fit$coefs), 1] - orig_link[, 1]
  orig_pred <- tibble::tibble(.pred_linear_pred = unname(orig_pred))
  tidy_pred <- predict(tidy_fit, house_sub, type = "linear_pred")
  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ordinalNet")
  skip_if_not_installed("QSARdata")

  onet_spec <-
    ordinal_reg(penalty = 0.01) |>
    set_mode("classification") |>
    set_engine("ordinalNet")
  expect_snapshot(onet_spec |> translate())

  expect_no_error({
    set.seed(13)
    onet_f_fit <- fit(onet_spec, class ~ ., data = caco_train)
  })

  expect_no_error({
    set.seed(13)
    onet_xy_fit <- fit_xy(onet_spec, x = caco_train[, -1], y = caco_train$class)
  })

  rownames(onet_f_fit$fit$args$x) <- NULL
  expect_equal(
    onet_f_fit$fit$coefs,
    onet_xy_fit$fit$coefs,
    tolerance = 0.001
  )
})

test_that("arguments agree", {
  skip_if_not_installed("ordinalNet")
  skip_if_not_installed("QSARdata")

  onet_arg_spec <-
    ordinal_reg(
      penalty = 0.1,
      mixture = .25,
      ordinal_link = "cloglog", odds_link = "stopping"
    ) |>
    set_mode("classification") |>
    set_engine("ordinalNet", path_values = 10 ^ seq(-6, -1))
  expect_snapshot(onet_arg_spec |> translate())

  expect_snapshot({
    set.seed(13)
    onet_arg_fit <- fit(onet_arg_spec, class ~ ., data = caco_train)
  })
  expect_equal(onet_arg_fit$fit$args$lambdaVals, 10 ^ seq(-6, -1))
  expect_equal(onet_arg_fit$fit$args$alpha, .25)
  expect_equal(onet_arg_fit$fit$args$link, "cloglog")
  expect_equal(onet_arg_fit$fit$args$family, "sratio")
})
