test_that("`threshold_structure` dial values and customization", {
  expect_equal(
    dials::threshold_structure()$values,
    dials::values_threshold_structure
  )
  expect_equal(dials::threshold_structure(letters[1:3])$values, letters[1:3])
})

test_that("VGAM `ordinal_link` values extend the bundled dial", {
  expect_true(all(dials::values_ordinal_link %in% values_ordinal_link_VGAM))
  expect_true(all(c("foldsqrt", "logc", "gord", "pord", "nbord") %in%
    values_ordinal_link_VGAM))
})

test_that("VGAM `threshold_structure` values extend the bundled dial", {
  expect_true(all(dials::values_threshold_structure %in%
    values_threshold_structure_VGAM))
  expect_true("qnorm" %in% values_threshold_structure_VGAM)
})

test_that("`parallel_reg` dial values and customization", {
  expect_equal(
    dials::parallel_reg()$values,
    dials::values_parallel_reg
  )
  expect_equal(dials::parallel_reg(TRUE)$values, TRUE)
})

test_that("`ordinalNet::ordinalNet` dial customization", {
  expect_equal(parallel_penalty_factor(range = c(-2, 2))$range,
               list(lower = -2, upper = 2))
})

test_that("`rpartScore::rpartScore` dial customization", {
  expect_equal(split_func(letters[1:3])$values, letters[1:3])
  expect_equal(prune_func(letters[1:3])$values, letters[1:3])
})

test_that("`ordinalForest::ordfor` dial customization", {
  expect_equal(naive_scores(TRUE)$values, TRUE)
  expect_equal(num_scores(3:4)$range, list(lower = 3L, upper = 4L))
  expect_equal(num_score_perms(100:101)$range, list(lower = 100L, upper = 101L))
  expect_equal(num_score_trees(c(10, 20))$range, list(lower = 10L, upper = 20L))
  expect_equal(num_scores_best(1:2)$range, list(lower = 1L, upper = 2L))
  expect_equal(ord_metric(letters[3:5])$values, letters[3:5])
})
