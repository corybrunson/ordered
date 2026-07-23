# Test model type and engine arguments here rather than in {parsnip} if they
# require engines to be loaded.

test_that("updating", {
  expect_snapshot(
    ordinal_reg(ordinal_link = "cauchit") |>
      set_engine("ordinalNet", standardize = FALSE) |>
      update(ordinal_link = tune(), standardize = tune())
  )
})

test_that("bad input", {
  expect_snapshot(
    error = TRUE,
    res <- translate(ordinal_reg(mode = "classification") |> set_engine(NULL))
  )
  expect_snapshot(error = TRUE, ordinal_reg(mode = "regression"))
  # NB: Behavior is different from that of most models because no engines are
  # contained in {parsnip}; see 'R/aaa_models.R' in `parsnip`.
  expect_snapshot(
    translate(ordinal_reg(mode = "classification") |> set_engine("wat?")),
    error = TRUE
  )
})

test_that("check_args() works", {
  skip_if_not_installed("parsnip", "1.2.1.9003")

  # Here for completeness, no checking is done
  expect_true(TRUE)
})

# parallel_reg validation ------------------------------------------------------

test_that("parallel_reg accepts logical input", {
  expect_snapshot(ordinal_reg(parallel_reg = TRUE))
  expect_snapshot(ordinal_reg(parallel_reg = FALSE))
})

test_that("parallel_reg accepts formula input", {
  expect_snapshot(ordinal_reg(parallel_reg = TRUE ~ x))
  expect_snapshot(ordinal_reg(parallel_reg = FALSE ~ y + z))
})

test_that("parallel_reg accepts list input", {
  expect_snapshot(ordinal_reg(parallel_reg = list(TRUE ~ x)))
  expect_snapshot(ordinal_reg(parallel_reg = list(FALSE ~ x, TRUE ~ y)))
  expect_snapshot(ordinal_reg(parallel_reg = list(FALSE, TRUE ~ y)))
})

test_that("parallel_reg rejects invalid inputs", {
  skip_if_not_installed("MASS")
  house_sub <- MASS::housing[rep(seq(nrow(MASS::housing)),
    MASS::housing$Freq), -5]

  # not logical or formula
  expect_snapshot(
    ordinal_reg(parallel_reg = "TRUE") |>
      set_engine("clm") |>
      fit(Sat ~ Infl + Cont, data = house_sub),
    error = TRUE
  )
  # formula without logical LHS
  expect_snapshot(
    ordinal_reg(parallel_reg = Sat ~ Infl) |>
      set_engine("clm") |>
      fit(Sat ~ Infl + Cont, data = house_sub),
    error = TRUE
  )
  # list with > 2 elements
  expect_snapshot(
    ordinal_reg(
      parallel_reg = list(TRUE ~ Infl, FALSE ~ Cont, TRUE)
    ) |>
      set_engine("clm") |>
      fit(Sat ~ Infl + Cont, data = house_sub),
    error = TRUE
  )
})
