# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ocf")
  house_sub <- get_house()$sub

  # replicate parsnip's formula preprocessing for a direct ocf call
  mod_frame <- model.frame(Sat ~ Infl + Type + Cont, data = house_sub)
  house_mat <- model.matrix(
    attr(mod_frame, "terms"), mod_frame,
    contrasts.arg = lapply(Filter(is.factor, mod_frame[-1L]), contr_onehot)
  )
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]

  set.seed(seed)
  orig_fit <- ocf::ocf(
    as.numeric(model.response(mod_frame)), house_mat,
    n.trees = 10, mtry = 4, min.node.size = 5,
    replace = FALSE, sample.fraction = 0.5,
    honesty = FALSE, inference = FALSE
  )

  tidy_spec <- rand_forest() |>
    set_engine("ocf") |>
    set_mode("classification") |>
    set_args(
      trees = 10, mtry = 4, min_n = 5,
      sample.fraction = 0.5, honesty = FALSE
    )
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Infl + Type + Cont, data = house_sub)

  orig_fit$tuning.info$call <- NULL
  tidy_fit$fit$tuning.info$call <- NULL
  expect_equal(orig_fit, tidy_fit$fit)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ocf")
  house_sub <- get_house()$sub
  house_train <- house_sub[seq(60), ]
  house_test <- house_sub[-seq(60), ]

  tidy_spec <- rand_forest(trees = 10) |>
    set_engine("ocf") |>
    set_mode("classification") |>
    set_args(sample.fraction = 0.5, honesty = FALSE)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Cont, data = house_train)

  house_frame <- model.frame(Sat ~ Infl + Type + Cont, data = house_test)
  house_mat <- model.matrix(
    attr(house_frame, "terms"), house_frame,
    contrasts.arg = lapply(Filter(is.factor, house_frame[-1L]), contr_onehot)
  )
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]

  orig_pred <- predict(tidy_fit$fit, data = house_mat, type = "response")
  orig_pred <- ordered(tidy_fit$lvl[orig_pred$classification], tidy_fit$lvl)
  orig_pred <- tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, house_test, type = "class")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ocf")
  house_sub <- get_house()$sub
  house_train <- house_sub[seq(60), ]
  house_test <- house_sub[-seq(60), ]

  tidy_spec <- rand_forest(trees = 10) |>
    set_engine("ocf") |>
    set_mode("classification") |>
    set_args(sample.fraction = 0.5, honesty = FALSE)
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Cont, data = house_train)

  house_frame <- model.frame(Sat ~ Infl + Type + Cont, data = house_test)
  house_mat <- model.matrix(
    attr(house_frame, "terms"), house_frame,
    contrasts.arg = lapply(Filter(is.factor, house_frame[-1L]), contr_onehot)
  )
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]

  orig_pred <- predict(tidy_fit$fit, data = house_mat, type = "response")
  orig_pred <- tibble::as_tibble(orig_pred$probabilities)
  names(orig_pred) <- paste0(".pred_", tidy_fit$lvl)
  tidy_pred <- predict(tidy_fit, house_test, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ocf")
  skip_if_not_installed("QSARdata")

  ocf_spec <- rand_forest() |>
    set_mode("classification") |>
    set_engine("ocf")
  expect_snapshot(ocf_spec |> translate())

  set.seed(seed)
  ocf_f_fit <- fit(ocf_spec, class ~ ., data = caco_train)
  set.seed(seed)
  ocf_xy_fit <-
    fit_xy(ocf_spec, x = caco_train[, -1], y = caco_train[["class"]])

  expect_equal(ocf_f_fit$fit$forests.info, ocf_xy_fit$fit$forests.info)
  expect_equal(ocf_f_fit$fit$importance, ocf_xy_fit$fit$importance)
  expect_equal(ocf_f_fit$fit$predictions, ocf_xy_fit$fit$predictions)

  f_info <- ocf_f_fit$fit$tuning.info
  xy_info <- ocf_xy_fit$fit$tuning.info
  f_info$call <- NULL
  xy_info$call <- NULL

  expect_equal(f_info, xy_info)
})

test_that("arguments agree", {
  skip_if_not_installed("ocf")
  skip_if_not_installed("QSARdata")

  ocf_arg_spec <-
    rand_forest(mtry = 2, min_n = 11, trees = 20) |>
    set_mode("classification") |>
    set_engine(
      "ocf",
      sample.fraction = 0.7, honesty = TRUE, replace = TRUE,
      alpha = 0.3, max.depth = 5
    )
  expect_snapshot(ocf_arg_spec |> translate())

  set.seed(seed)
  ocf_arg_fit <- fit(ocf_arg_spec, class ~ ., data = caco_train)

  info <- ocf_arg_fit$fit$tuning.info
  expect_equal(info$n.trees, 20)
  expect_equal(info$mtry, 2)
  expect_equal(info$min.node.size, 11)
  expect_equal(info$call$sample.fraction, 0.7)
  expect_equal(info$honesty, TRUE)
  expect_equal(info$replace, TRUE)
  expect_equal(info$call$alpha, 0.3)
  expect_equal(info$call[["max.depth"]], 5)
  # defaulted in registration
  expect_equal(info$call$n.threads, 1)
})

test_that("wrapper validates the balance parameter", {
  skip_if_not_installed("ocf")

  x <- matrix(seq(20), ncol = 2)
  y <- factor(rep(c("a", "b"), 10))

  expect_error(
    ocf_wrapper(X = x, Y = y, n.trees = 1, alpha = 0.6),
    "alpha"
  )
})

test_that("engine arguments are registered", {
  skip_if_not_installed("ocf")
  prms <-
    parsnip::rand_forest(engine = "ocf", mode = "classification") |>
    parsnip:::tunable.model_spec() |>
    dplyr::filter(component_id == "engine")
  expect_equal(nrow(prms), 6L)

  for (i in seq_len(nrow(prms))) {
    tmp <- prms$call_info[[i]]
    tmp$argument <- prms$name[i]
    expect_snapshot(print(unlist(tmp)))
  }
})
