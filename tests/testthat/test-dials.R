test_that("threshold_structure dial values and customization", {
  expect_equal(
    dials::threshold_structure()$values,
    dials::values_threshold_structure
  )
  expect_equal(dials::threshold_structure(letters[1:3])$values, letters[1:3])
})

test_that("parallel_reg dial values and customization", {
  expect_equal(
    dials::parallel_reg()$values,
    dials::values_parallel_reg
  )
  expect_equal(dials::parallel_reg(TRUE)$values, TRUE)
})

test_that("ordinalNet::ordinalNet dial customization", {
  expect_equal(parallel_penalty_factor(range = c(-2, 2))$range,
               list(lower = -2, upper = 2))
})

test_that("rpartScore::rpartScore dial customization", {
  expect_equal(split_func(letters[1:3])$values, letters[1:3])
  expect_equal(prune_func(letters[1:3])$values, letters[1:3])
})

test_that("ordinalForest::ordfor dial customization", {
  expect_equal(naive_scores(TRUE)$values, TRUE)
  expect_equal(num_scores(3:4)$range, list(lower = 3L, upper = 4L))
  expect_equal(num_score_perms(100:101)$range, list(lower = 100L, upper = 101L))
  expect_equal(num_score_trees(c(10, 20))$range, list(lower = 10L, upper = 20L))
  expect_equal(num_scores_best(1:2)$range, list(lower = 1L, upper = 2L))
  expect_equal(ord_metric(letters[3:5])$values, letters[3:5])
})
