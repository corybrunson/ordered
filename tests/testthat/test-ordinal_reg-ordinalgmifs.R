# model: basic -----------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("ordinalgmifs")
  ex <- get_ordinalgmifs_ex()

  orig_fit <- ordinalgmifs::ordinalgmifs(
    y ~ z,
    data = ex$data,
    x = ex$x
  )

  tidy_spec <- ordinal_reg() |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification")
  tidy_fit <- fit(tidy_spec, y ~ x1 + x2 + z, data = ex$data)

  tidy_fit$fit$.ordered_x_names <- NULL
  tidy_fit$fit$.ordered_neww <- NULL
  tidy_fit$fit$.ordered_model_select <- NULL
  expect_equal(orig_fit, tidy_fit$fit)
})

# prediction: class ------------------------------------------------------------

test_that("class prediction", {
  skip_if_not_installed("ordinalgmifs")
  ex <- get_ordinalgmifs_ex()

  tidy_spec <- ordinal_reg() |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification")
  tidy_fit <- fit(tidy_spec, y ~ x1 + x2 + z, data = ex$data)

  orig_pred <- predict(
    tidy_fit$fit,
    neww = ~z,
    newdata = ex$data,
    newx = ex$x
  )
  orig_pred <- factor(orig_pred$class, levels = levels(ex$data$y))
  orig_pred <- tibble::tibble(.pred_class = orig_pred)
  tidy_pred <- predict(tidy_fit, ex$data)
  expect_equal(tidy_pred, orig_pred)
})

# prediction: probability ------------------------------------------------------

test_that("probability prediction", {
  skip_if_not_installed("ordinalgmifs")
  ex <- get_ordinalgmifs_ex()

  tidy_spec <- ordinal_reg(odds_link = "stereotype") |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification")
  tidy_fit <- fit(tidy_spec, y ~ x1 + x2 + z, data = ex$data)

  orig_pred <- predict(
    tidy_fit$fit,
    neww = ~z,
    newdata = ex$data,
    newx = ex$x
  )
  orig_pred <- orig_pred$predicted
  colnames(orig_pred) <- levels(ex$data$y)
  orig_pred <- tibble::as_tibble(orig_pred)
  orig_pred <- set_names(orig_pred, paste0(".pred_", levels(ex$data$y)))
  tidy_pred <- predict(tidy_fit, ex$data, type = "prob")
  expect_equal(tidy_pred, orig_pred)
})

# argument handling ------------------------------------------------------------

test_that("link values are matched to native values", {
  expect_equal(
    unname(vapply(
      c(
        "cumulative_link",
        "adjacent_categories",
        "stopping_ratio",
        "stereotype"
      ),
      match_odds_link_ordinalgmifs,
      character(1)
    )),
    c("Cumulative", "AdjCategory", "ForwardCR", "Stereotype")
  )

  expect_equal(
    unname(vapply(
      c("stopping_ratio", "ForwardCR", "BackwardCR"),
      \(x) match_odds_link_ordinalgmifs(x, reverse = TRUE),
      character(1)
    )),
    c("BackwardCR", "BackwardCR", "ForwardCR")
  )

  expect_equal(
    unname(vapply(
      c("logistic", "probit", "cloglog", "loge"),
      match_ordinal_link_ordinalgmifs,
      character(1)
    )),
    c("logit", "probit", "cloglog", "loge")
  )

  expect_equal(match_odds_link_ordinalgmifs("AdjCategory"), "AdjCategory")
  expect_equal(match_ordinal_link_ordinalgmifs("logit"), "logit")
})

test_that("unsupported odds links and reverse combinations error", {
  expect_snapshot(
    match_odds_link_ordinalgmifs("continuation_ratio"),
    error = TRUE
  )
  expect_snapshot(
    match_odds_link_ordinalgmifs("continuation_ratio", reverse = TRUE),
    error = TRUE
  )
  expect_snapshot(
    match_odds_link_ordinalgmifs("cumulative_link", reverse = TRUE),
    error = TRUE
  )
  expect_snapshot(
    match_odds_link_ordinalgmifs("Stereotype", reverse = TRUE),
    error = TRUE
  )
  expect_snapshot(
    match_odds_link_ordinalgmifs("stopping_ratio", reverse = NA),
    error = TRUE
  )
})

test_that("stopping ratio direction is translated", {
  skip_if_not_installed("ordinalgmifs")
  ex <- get_ordinalgmifs_ex()
  link <- "cloglog"
  strip <- function(x) {
    x$.ordered_x_names <- NULL
    x$.ordered_neww <- NULL
    x$.ordered_model_select <- NULL
    x
  }

  forward <- ordinal_reg(
    ordinal_link = link,
    odds_link = "stopping_ratio"
  ) |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification") |>
    fit(y ~ x1 + x2 + z, data = ex$data)
  expect_equal(
    strip(forward$fit),
    ordinalgmifs::ordinalgmifs(
      y ~ z,
      data = ex$data,
      x = ex$x,
      probability.model = "ForwardCR",
      link = link
    )
  )

  backward <- ordinal_reg(
    ordinal_link = link,
    odds_link = "stopping_ratio"
  ) |>
    set_engine("ordinalgmifs", x = c("x1", "x2"), reverse = TRUE) |>
    set_mode("classification") |>
    fit(y ~ x1 + x2 + z, data = ex$data)
  expect_equal(
    strip(backward$fit),
    ordinalgmifs::ordinalgmifs(
      y ~ z,
      data = ex$data,
      x = ex$x,
      probability.model = "BackwardCR",
      link = link
    )
  )

  flipped <- ordinalgmifs_wrapper(
    y ~ x1 + x2 + z,
    ex$data,
    x = c("x1", "x2"),
    probability.model = "ForwardCR",
    link = link,
    reverse = TRUE
  )
  expect_equal(
    strip(flipped),
    ordinalgmifs::ordinalgmifs(
      y ~ z,
      data = ex$data,
      x = ex$x,
      probability.model = "BackwardCR",
      link = link
    )
  )
})

test_that("penalized predictors are resolved", {
  ex <- get_ordinalgmifs_ex()

  expect_equal(ordinalgmifs_x_names(NULL, ex$data), character())
  expect_equal(ordinalgmifs_x_names(c("x2", "z"), ex$data), c("x2", "z"))
  expect_equal(
    ordinalgmifs_x_names(ex$data[, c("x1", "x2")], ex$data),
    c("x1", "x2")
  )
  expect_error(
    ordinalgmifs_x_names("nope", ex$data),
    "not found"
  )
})

# translation & interfaces -----------------------------------------------------

test_that("interfaces agree", {
  skip_if_not_installed("ordinalgmifs")
  ex <- get_ordinalgmifs_ex()

  spec <- ordinal_reg() |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification")

  f_fit <- fit(spec, y ~ ., data = ex$data)
  xy_fit <- fit_xy(spec, x = ex$data[, -1], y = ex$data$y)

  expect_equal(
    predict(f_fit, ex$data, type = "prob"),
    predict(xy_fit, ex$data, type = "prob")
  )
})

test_that("translation", {
  spec <- ordinal_reg(
    ordinal_link = "logistic",
    odds_link = "stereotype"
  ) |>
    set_engine("ordinalgmifs", x = c("x1", "x2")) |>
    set_mode("classification")
  expect_snapshot(spec |> translate())

  reverse_spec <- ordinal_reg(
    ordinal_link = "cloglog",
    odds_link = "stopping_ratio"
  ) |>
    set_engine("ordinalgmifs", x = c("x1", "x2"), reverse = TRUE) |>
    set_mode("classification")
  expect_snapshot(reverse_spec |> translate())
})
