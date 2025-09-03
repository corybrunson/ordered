# Test model type and engine arguments here rather than in {parsnip} if they
# require engines to be loaded.

test_that("check_args() works", {
  skip_if_not_installed("parsnip", "1.2.1.9003")

  # Here for completeness, no checking is done
  expect_true(TRUE)
})
