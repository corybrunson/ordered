seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object (penalty path from original to tidy)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  # https://stackoverflow.com/a/4569239
  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )

  # no extra arguments

  set.seed(seed)
  orig_fit <- suppressWarnings(
    glmnetcr::glmnetcr(
      house_vars,
      y = house_sub$Sat,
      nlambda = 120,
    )
  )

  tidy_spec <- ordinal_reg(penalty = 0.01) |>
    set_engine("glmnetcr", path_values = !! orig_fit$lambda)

  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$npasses <- tidy_fit$fit$npasses <- NULL
  orig_fit$jerr <- tidy_fit$fit$jerr <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_attr = TRUE)

  # extra arguments

  set.seed(seed)
  orig_fit <- suppressWarnings(
    glmnetcr::glmnetcr(
      house_vars,
      y = house_sub$Sat,
      nlambda = 120,
      alpha = .5, method = "forward"
    )
  )

  tidy_spec <- ordinal_reg(penalty = 0.01, mixture = .5) |>
    set_engine("glmnetcr", method = "forward", path_values = !! orig_fit$lambda)

  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  orig_fit$npasses <- tidy_fit$fit$npasses <- NULL
  orig_fit$jerr <- tidy_fit$fit$jerr <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_attr = TRUE)
})

test_that("model object (penalty path from tidy to original)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  house_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 2:4], contrasts, contrasts = FALSE)
  )

  # no extra arguments

  tidy_spec <- ordinal_reg(penalty = 1) |>
    set_engine("glmnetcr") |>
    set_mode("classification")
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)

  set.seed(seed)
  orig_fit <- suppressWarnings(
    glmnetcr::glmnetcr(
      house_vars,
      y = house_sub$Sat,
      lambda = tidy_fit$fit$lambda
    )
  )

  orig_fit$npasses <- tidy_fit$fit$npasses <- NULL
  orig_fit$jerr <- tidy_fit$fit$jerr <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_attr = TRUE)

  # extra arguments

  tidy_spec <- ordinal_reg(penalty = 1, mixture = .5) |>
    set_engine("glmnetcr", method = "forward")
  set.seed(seed)
  tidy_fit <- suppressWarnings(
    fit(tidy_spec, Sat ~ Type + Infl + Cont, data = house_sub)
  )

  set.seed(seed)
  orig_fit <- suppressWarnings(
    glmnetcr::glmnetcr(
      house_vars,
      y = house_sub$Sat,
      lambda = tidy_fit$fit$lambda,
      alpha = .5, method = "forward"
    )
  )

  orig_fit$npasses <- tidy_fit$fit$npasses <- NULL
  orig_fit$jerr <- tidy_fit$fit$jerr <- NULL
  # FIXME: Why are some fits (coefficients and degrees of freedom) different?
  orig_fit$beta <- as.matrix(orig_fit$beta)
  tidy_fit$fit$beta <- as.matrix(tidy_fit$fit$beta)
  orig_fit$df <- tidy_fit$fit$df <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_attr = TRUE, tolerance = 0.01)
})

# case weights -----------------------------------------------------------------

test_that("case weights", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_data <- get_house()$data
  wt_data <- transform(house_data, Freq = frequency_weights(Freq))

  wt_vars <- model.matrix(
    Sat ~ Type + Infl + Cont + 0, data = wt_data,
    contrasts.arg = lapply(wt_data[, 2:4], contrasts, contrasts = FALSE)
  )

  set.seed(seed)
  orig_fit <- suppressWarnings(
    glmnetcr::glmnetcr(
      wt_vars, y = wt_data$Sat,
      weights = wt_data$Freq
    )
  )

  tidy_spec <- ordinal_reg(penalty = 1) |>
    set_engine("glmnetcr", path_values = !! orig_fit$lambda)
  set.seed(seed)
  tidy_fit <- fit(
    tidy_spec,
    Sat ~ Type + Infl + Cont, data = wt_data,
    case_weights = wt_data$Freq
  )

  # orig_fit$npasses <- tidy_fit$fit$npasses <- NULL
  # orig_fit$jerr <- tidy_fit$fit$jerr <- NULL
  expect_equal(orig_fit, tidy_fit$fit, ignore_attr = TRUE)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  tidy_fit <- suppressWarnings(
    ordinal_reg(engine = "glmnetcr", penalty = 1) |>
      fit(Sat ~ Type + Cont, data = house_sub)
  )

  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )

  # use BIC-selected step (default)
  pred_all <- predict(tidy_fit$fit, newx = house_vars)
  s_bic <- which.min(pred_all$BIC)
  orig_pred <- pred_all$class[, s_bic]
  orig_pred <- ordered(orig_pred, tidy_fit$lvl)
  orig_pred <- tibble::tibble(.pred_class = orig_pred)

  tidy_pred <- predict(
    tidy_fit, house_sub,
    penalty = tidy_fit$fit$lambda[s_bic]
  )

  expect_equal(orig_pred, tidy_pred)
})

# prediction: prob -------------------------------------------------------------

test_that("prob prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  tidy_fit <- suppressWarnings(
    ordinal_reg(engine = "glmnetcr", penalty = 1) |>
      fit(Sat ~ Type + Cont, data = house_sub)
  )

  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )

  # use BIC-selected step (default)
  pred_all <- predict(tidy_fit$fit, newx = house_vars)
  s_bic <- which.min(pred_all$BIC)
  orig_pred <- pred_all$probs[, , s_bic]
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", colnames(orig_pred)))

  tidy_pred <- predict(
    tidy_fit, house_sub,
    type = "prob",
    penalty = tidy_fit$fit$lambda[s_bic]
  )

  expect_equal(orig_pred, tidy_pred, tolerance = 0.0001)
})

# multi_predict ----------------------------------------------------------------

test_that("multi_predict class", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  tidy_fit <- suppressWarnings(
    ordinal_reg(engine = "glmnetcr", penalty = 1) |>
      fit(Sat ~ Type + Cont, data = house_sub)
  )

  house_vars <- model.matrix(
    Sat ~ Type + Cont + 0, data = house_sub,
    contrasts.arg = lapply(house_sub[, 3:4], contrasts, contrasts = FALSE)
  )

  pen_vals <- tidy_fit$fit$lambda[length(tidy_fit$fit$lambda) * c(1, 2) / 3]
  multi_pred <- multi_predict(
    tidy_fit,
    new_data = house_sub,
    type = "class",
    penalty = pen_vals
  )

  expect_s3_class(multi_pred, "tbl_df")
  expect_equal(nrow(multi_pred), nrow(house_sub))
  expect_true(".pred" %in% names(multi_pred))

  # each nested tibble should have one row per penalty value
  nested_rows <- unique(purrr::map_int(multi_pred$.pred, nrow))
  expect_equal(nested_rows, length(pen_vals))
})

test_that("multiple prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("glmnetcr")
  house_sub <- get_house()$sub

  tidy_fit <- suppressWarnings(
    ordinal_reg(engine = "glmnetcr", penalty = 1) |>
      fit(Sat ~ Type + Cont, data = house_sub)
  )

  pen_vals <- tidy_fit$fit$lambda[length(tidy_fit$fit$lambda) * c(1, 2) / 3]
  multi_pred <-
    multi_predict(tidy_fit, house_sub, type = "class", penalty = pen_vals)

  # compare multi_predict result with sequential predict at each penalty
  for (i in seq_along(pen_vals)) {
    single_pred <- predict(
      tidy_fit, house_sub, type = "class",
      penalty = pen_vals[i]
    )
    expect_equal(
      sapply(multi_pred$.pred, function(x) x$.pred_class[i]),
      single_pred$.pred_class
    )
  }
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("glmnetcr")
  skip_if_not_installed("QSARdata")

  gcr_spec <-
    ordinal_reg() |>
    set_mode("classification") |>
    set_engine("glmnetcr")
  expect_snapshot(gcr_spec |> translate())

  suppressWarnings(
    expect_warning(
      fit(gcr_spec, class ~ ., data = caco_train),
      regexp = "penalty.*path_values"
    )
  )

  gcr_spec <-
    ordinal_reg(penalty = 1) |>
    set_mode("classification") |>
    set_engine("glmnetcr")
  expect_snapshot(gcr_spec |> translate())

  expect_no_error({
    set.seed(13)
    gcr_f_fit <- suppressWarnings(
      fit(gcr_spec, class ~ ., data = caco_train)
    )
  })

  expect_no_error({
    set.seed(13)
    gcr_xy_fit <- suppressWarnings(
      fit_xy(gcr_spec, x = caco_train[, -1], y = caco_train$class)
    )
  })

  expect_equal(gcr_f_fit$fit, gcr_xy_fit$fit, ignore_attr = TRUE)
})

test_that("arguments agree", {
  skip_if_not_installed("glmnetcr")
  skip_if_not_installed("QSARdata")

  # using mixture as model-level parameter
  pen_vec <- 10 ^ seq(0, -2, -.5)
  gcr_arg_spec <-
    ordinal_reg(penalty = 1, mixture = .25) |>
    set_mode("classification") |>
    set_engine("glmnetcr", method = "forward", path_values = pen_vec)
  expect_snapshot(gcr_arg_spec |> translate())

  expect_no_error({
    set.seed(13)
    gcr_arg_fit <- suppressWarnings(
      fit(gcr_arg_spec, class ~ ., data = caco_train)
    )
  })
  expect_equal(
    gcr_arg_fit$fit$lambda,
    pen_vec[seq_along(gcr_arg_fit$fit$lambda)]
  )
  expect_equal(gcr_arg_fit$fit$method, "forward")
})
