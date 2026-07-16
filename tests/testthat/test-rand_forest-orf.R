seed <- 144688L

# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("orf")
  house_sub <- get_house()$sub

  # replicate parsnip's formula preprocessing for a direct orf call
  mod_frame <- model.frame(Sat ~ Infl + Type + Cont, data = house_sub)
  mod_terms <- attr(mod_frame, "terms")
  house_mat <- model.matrix(mod_terms, mod_frame)
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]
  house_vec <- as.numeric(model.response(mod_frame))

  set.seed(seed)
  orig_fit <- orf::orf(
    house_mat, house_vec,
    num.trees = 10, mtry = 4, min.node.size = 5,
    replace = FALSE, sample.fraction = 0.5,
    honesty = FALSE, inference = FALSE, importance = FALSE
  )

  tidy_spec <- rand_forest() |>
    set_engine("orf") |>
    set_mode("classification") |>
    set_args(
      trees = 10, mtry = 4, min_n = 5,
      sample.fraction = 0.5, honesty = FALSE
    )
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Infl + Type + Cont, data = house_sub)

  expect_equal(orig_fit, tidy_fit$fit)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("orf")
  house_sub <- get_house()$sub
  house_train <- house_sub[seq(60), ]
  house_test <- house_sub[-seq(60), ]

  tidy_spec <- rand_forest(trees = 10) |>
    set_engine("orf") |>
    set_mode("classification") |>
    set_args(
      sample.fraction = 0.5, honesty = FALSE
    )
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Cont, data = house_train)

  house_frame <- model.frame(Sat ~ Type + Cont, data = house_test)
  house_terms <- attr(house_frame, "terms")
  house_mat <- model.matrix(house_terms, house_frame)
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]

  orig_pred <- predict(tidy_fit$fit, newdata = house_mat, type = "c")
  orig_pred <- ordered(tidy_fit$lvl[orig_pred$predictions], tidy_fit$lvl)
  orig_pred <- tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, house_test, type = "class")

  expect_equal(orig_pred, tidy_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("orf")
  house_sub <- get_house()$sub
  house_train <- house_sub[seq(60), ]
  house_test <- house_sub[-seq(60), ]

  tidy_spec <- rand_forest(trees = 10) |>
    set_engine("orf") |>
    set_mode("classification") |>
    set_args(
      sample.fraction = 0.5, honesty = FALSE
    )
  set.seed(seed)
  tidy_fit <- fit(tidy_spec, Sat ~ Type + Cont, data = house_train)

  house_frame <- model.frame(Sat ~ Type + Cont, data = house_test)
  house_terms <- attr(house_frame, "terms")
  house_mat <- model.matrix(house_terms, house_frame)
  house_mat <- house_mat[, colnames(house_mat) != "(Intercept)", drop = FALSE]

  orig_pred <- predict(tidy_fit$fit, newdata = house_mat, type = "p")
  orig_pred <- tibble::as_tibble(as.data.frame(orig_pred$predictions))
  names(orig_pred) <- paste0(".pred_", tidy_fit$lvl)
  tidy_pred <- predict(tidy_fit, house_test, type = "prob")

  expect_equal(orig_pred, tidy_pred)
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("orf")
  skip_if_not_installed("QSARdata")

  orf_spec <-
    rand_forest() |>
    set_mode("classification") |>
    set_engine("orf")
  expect_snapshot(orf_spec |> translate())

  expect_no_error({
    set.seed(13)
    orf_f_fit <- fit(orf_spec, class ~ ., data = caco_train)
  })
  expect_snapshot(orf_f_fit)

  expect_no_error({
    set.seed(13)
    orf_xy_fit <-
      fit_xy(orf_spec, x = caco_train[, -1], y = caco_train[["class"]])
  })
  expect_snapshot(orf_xy_fit)

  expect_equal(
    orf_f_fit$spec$args,
    orf_xy_fit$spec$args
  )
  expect_equal(
    orf_f_fit$spec$eng_args,
    orf_xy_fit$spec$eng_args
  )
  expect_equal(
    orf_f_fit$fit$forests,
    orf_xy_fit$fit$forests
  )
  expect_equal(
    orf_f_fit$fit$info,
    orf_xy_fit$fit$info,
    ignore_attr = TRUE
  )
})

test_that("arguments agree", {
  skip_if_not_installed("orf")
  skip_if_not_installed("QSARdata")

  orf_arg_spec <-
    rand_forest(mtry = 2, min_n = 11, trees = 100) |>
    set_mode("classification") |>
    set_engine(
      "orf",
      sample.fraction = 0.7, honesty = TRUE, honesty.fraction = 0.4
    )
  expect_snapshot(orf_arg_spec |> translate())

  expect_snapshot({
    set.seed(13)
    orf_arg_fit <- fit(orf_arg_spec, class ~ ., data = caco_train)
  })

  # check that model args are passed through
  expect_equal(orf_arg_fit$fit$info$inputs$mtry, 2)
  expect_equal(orf_arg_fit$fit$info$inputs$min.node.size, 11)
  expect_equal(orf_arg_fit$fit$info$inputs$num.trees, 100)
  # check that engine args are passed through
  expect_equal(orf_arg_fit$fit$info$inputs$sample.fraction, 0.7)
  expect_equal(orf_arg_fit$fit$info$inputs$honesty, TRUE)
  expect_equal(orf_arg_fit$fit$info$inputs$honesty.fraction, 0.4)
})

test_that("engine arguments are registered", {
  prms <-
    parsnip::rand_forest(engine = "orf", mode = "classification") |>
    parsnip:::tunable.model_spec() |>
    dplyr::filter(component_id == "engine")
  expect_true(nrow(prms) == 3L)
  for (i in 1:nrow(prms)) {
    tmp <- prms$call_info[[i]]
    tmp$argument <- prms$name[i]
    expect_snapshot(print(unlist(tmp)))
  }
})
