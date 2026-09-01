test_that("clm_formulas() splits a formula into location and nominal parts", {
  d <- data.frame(
    rating = ordered(rep(1:3, 4)),
    temp = rep(c("cold", "warm"), 6),
    contact = rep(c("no", "yes"), each = 6)
  )

  res <- clm_formulas(rating ~ temp + contact, d)

  expect_equal(res$formula, rating ~ 1)
  expect_equal(res$nominal, ~ temp + contact)
})

test_that("clm_formulas() expands a dot without capturing the outcome", {
  d <- data.frame(
    rating = ordered(rep(1:3, 4)),
    temp = rep(c("cold", "warm"), 6),
    contact = rep(c("no", "yes"), each = 6)
  )

  res <- clm_formulas(rating ~ ., d)

  expect_equal(res$formula, rating ~ 1)
  expect_equal(res$nominal, ~ temp + contact)
})

test_that("clm_formulas() preserves the formula environment", {
  d <- data.frame(rating = ordered(rep(1:3, 4)), temp = rep(c("a", "b"), 6))
  f <- rating ~ temp

  res <- clm_formulas(f, d)

  expect_identical(rlang::f_env(res$nominal), rlang::f_env(f))
})

test_that("clm_formulas() keeps an offset in the location formula only", {
  d <- data.frame(
    rating = ordered(rep(1:3, 4)),
    temp = rep(c("a", "b"), 6),
    off = seq_len(12) / 12
  )

  res <- clm_formulas(rating ~ temp + offset(off), d)

  expect_equal(res$formula, rating ~ offset(off))
  expect_equal(res$nominal, ~temp)
})

test_that("clm_formulas() carries transformations and odd names through", {
  d <- data.frame(rating = ordered(rep(1:3, 4)), x = seq_len(12) / 12)
  d[["not valid"]] <- rep(c("a", "b"), 6)

  res <- clm_formulas(rating ~ `not valid` + log(x) + poly(x, 2), d)

  expect_equal(
    rlang::f_rhs(res$nominal),
    rlang::expr(`not valid` + log(x) + poly(x, 2))
  )
})

test_that("clm_formulas() rejects a formula with no predictors", {
  d <- data.frame(rating = ordered(rep(1:3, 4)))

  expect_snapshot(error = TRUE, {
    clm_formulas(rating ~ 1, d)
  })
})

test_that("clm_wrapper() moves every predictor to nominal when not parallel", {
  skip_if_not_installed("ordinal")
  soup <- ordinal::soup

  res <- clm_wrapper(SURENESS ~ PROD + DAY, data = soup, parallel_reg = FALSE)
  exp <- ordinal::clm(SURENESS ~ 1, nominal = ~ PROD + DAY, data = soup)

  expect_equal(coef(res), coef(exp))
  expect_equal(logLik(res), logLik(exp))
})

test_that("clm_wrapper() fits a plain model when parallel", {
  skip_if_not_installed("ordinal")
  soup <- ordinal::soup
  exp <- ordinal::clm(SURENESS ~ PROD + DAY, data = soup)

  expect_equal(coef(clm_wrapper(SURENESS ~ PROD + DAY, data = soup)), coef(exp))
  expect_equal(
    coef(clm_wrapper(SURENESS ~ PROD + DAY, data = soup, parallel_reg = TRUE)),
    coef(exp)
  )
})

test_that("clm_wrapper() records the formulas it used in the call", {
  skip_if_not_installed("ordinal")
  soup <- ordinal::soup

  res <- clm_wrapper(SURENESS ~ PROD + DAY, data = soup, parallel_reg = FALSE)
  expect_equal(res$call$formula, SURENESS ~ 1)
  expect_equal(res$call$nominal, ~ PROD + DAY)

  res <- clm_wrapper(SURENESS ~ PROD + DAY, data = soup)
  expect_equal(res$call$formula, SURENESS ~ PROD + DAY)
  expect_null(res$call$nominal)
})

test_that("clm_wrapper() passes `nominal` through for partial parallelism", {
  skip_if_not_installed("ordinal")
  soup <- ordinal::soup

  res <- clm_wrapper(SURENESS ~ PROD, data = soup, nominal = ~DAY)
  exp <- ordinal::clm(SURENESS ~ PROD, nominal = ~DAY, data = soup)

  expect_equal(coef(res), coef(exp))
})

test_that("clm_wrapper() records arguments passed through `...` in the call", {
  skip_if_not_installed("ordinal")
  soup <- ordinal::soup

  res <- clm_wrapper(
    SURENESS ~ PROD,
    data = soup,
    parallel_reg = FALSE,
    link = "probit"
  )
  expect_equal(res$call$link, "probit")

  res <- clm_wrapper(SURENESS ~ PROD, data = soup, nominal = ~DAY)
  expect_equal(res$call$nominal, rlang::expr(~DAY))

  # recorded as written, so that bulky arguments do not bloat the call
  res <- clm_wrapper(
    SURENESS ~ PROD,
    data = soup,
    control = ordinal::clm.control(maxIter = 200)
  )
  expect_equal(
    res$call$control,
    rlang::expr(ordinal::clm.control(maxIter = 200))
  )
})
