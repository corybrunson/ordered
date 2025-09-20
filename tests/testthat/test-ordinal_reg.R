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
    translate(ordinal_reg(mode = "classification") |> set_engine("wat?"))
  )
})

test_that("check_args() works", {
  skip_if_not_installed("parsnip", "1.2.1.9003")

  # Here for completeness, no checking is done
  expect_true(TRUE)
})
