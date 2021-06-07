# control_quality ----

test_that("control_quality function works as expected with default settings", {
  result <- control_quality()
  expect_is(result, "list")
  expect_named(result, c("min_cpm", "min_readcount_prop", "min_corr", "min_depth"))
})

test_that("control_quality function works as expected with custom settings", {
  result <- expect_silent(control_quality(min_cpm = 5, min_readcount_prop = .001, min_corr = .1, min_depth = 3))
  expect_is(result, "list")
  expect_identical(result$min_cpm, 5)
  expect_identical(result$min_readcount_prop, .001)
  expect_identical(result$min_corr, .1)
  expect_identical(result$min_depth, 3)
})

test_that("control_quality fails as expected with invalid settings", {
  expect_error(control_quality(min_cpm = -1))
  expect_error(control_quality(min_cpm = TRUE))
  expect_error(control_quality(min_cpm = "fail"))
  expect_error(control_quality(min_cpm = c(1, 2)))
  
  expect_error(control_quality(min_readcount_prop = -1))
  expect_error(control_quality(min_readcount_prop = TRUE))
  expect_error(control_quality(min_readcount_prop = "fail"))
  expect_error(control_quality(min_readcount_prop = 5))
  expect_error(control_quality(min_readcount_prop = c(1, 2)))
  
  expect_error(control_quality(min_corr = -1))
  expect_error(control_quality(min_corr = TRUE))
  expect_error(control_quality(min_corr = "fail"))
  expect_error(control_quality(min_corr = 5))
  expect_error(control_quality(min_corr = c(1, 2)))
  
  expect_error(control_quality(min_depth = -1))
  expect_error(control_quality(min_depth = TRUE))
  expect_error(control_quality(min_depth = "fail"))
  expect_error(control_quality(min_depth = c(1, 2)))
})

# h_low_expression_flag ----
test_that("h_low_expression_flag function works as expected with default settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  control <- control_quality()
  result <- expect_silent(h_low_expression_flag(object, control))
  expect_is(result, "logical")
  expect_equal(length(result), nrow(object))
})

test_that("h_low_expression_flag function works as expected with custom settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  control <- control_quality(min_cpm = 5, min_readcount_prop = 0.5, min_corr = .1, min_depth = 3)
  result <- expect_silent(h_low_expression_flag(object, control))
  expect_is(result, "logical")
  expect_equal(length(result), nrow(object))
})

test_that("h_low_expression_flag fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- expect_silent(HermesData(get_se()))
  cont1 <- control_quality()
  cont2 <- list(1, 2, 3, 4)
  expect_error(h_low_expression_flag(object1, cont1))
  expect_error(h_low_expression_flag(object3, cont2))
  expect_error(h_low_expression_flag(object2, cont1))
  expect_error(h_low_expression_flag(object2, cont2))
})

# h_tech_failure_flag ----
test_that("h_tech_failure_flag function works as expected with default settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(h_tech_failure_flag(object))
  expect_is(result, "logical")
  expect_equal(length(result), ncol(object))
})

test_that("h_tech_failure_flag function works as expected with custom settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  control <- control_quality(min_corr = 0.35)
  result <- expect_silent(h_tech_failure_flag(object, control))
  expect_is(result, "logical")
  expect_equal(length(result), ncol(object))
})

test_that("h_tech_failure_flag fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  object3 <- expect_silent(HermesData(get_se()))
  cont1 <- "0.5"
  cont2 <- list(1, 2, 3, 4)
  expect_error(h_tech_failure_flag(object1, cont1))
  expect_error(h_tech_failure_flag(object3, cont2))
  expect_error(h_tech_failure_flag(object2, cont1))
  expect_error(h_tech_failure_flag(object2, cont2))
})
