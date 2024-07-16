# control_quality ----

test_that("control_quality function works as expected with default settings", {
  result <- control_quality()
  expect_list(result, len = 4)
  expect_named(result, c("min_cpm", "min_cpm_prop", "min_corr", "min_depth"))
})

test_that("control_quality function works as expected with custom settings", {
  result <- expect_silent(control_quality(min_cpm = 5, min_cpm_prop = .001, min_corr = .1, min_depth = 3))
  expect_list(result, len = 4)
  expect_identical(result$min_cpm, 5)
  expect_identical(result$min_cpm_prop, .001)
  expect_identical(result$min_corr, .1)
  expect_identical(result$min_depth, 3)
})

test_that("control_quality fails as expected with invalid settings", {
  expect_error(control_quality(min_cpm = -1))
  expect_error(control_quality(min_cpm = TRUE))
  expect_error(control_quality(min_cpm = "fail"))
  expect_error(control_quality(min_cpm = c(1, 2)))

  expect_error(control_quality(min_cpm_prop = -1))
  expect_error(control_quality(min_cpm_prop = TRUE))
  expect_error(control_quality(min_cpm_prop = "fail"))
  expect_error(control_quality(min_cpm_prop = 5))
  expect_error(control_quality(min_cpm_prop = c(1, 2)))

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

# add_quality_flags ----

test_that("add_quality_flags function works as expected with default settings", {
  object <- hermes_data
  control <- control_quality()
  result <- expect_silent(add_quality_flags(object, control))
  expect_s4_class(result, "HermesData")
})

test_that("add_quality_flags function works as expected with custom settings", {
  object <- hermes_data
  control <- control_quality(min_cpm = 10, min_cpm_prop = 0.2, min_corr = 0.3, min_depth = 5)
  result <- expect_silent(add_quality_flags(object, control))
  expect_s4_class(result, "HermesData")
})

test_that("add_quality_flags fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  object3 <- expect_silent(HermesData(get_se()))
  cont1 <- "0.5"
  cont2 <- list(1, 2, 3, 4)
  expect_error(add_quality_flags(object1, cont1))
  expect_error(add_quality_flags(object3, cont2))
  expect_error(add_quality_flags(object2, cont1))
  expect_error(add_quality_flags(object2, cont2))
})

# h_low_expression_flag ----

test_that("h_low_expression_flag function works as expected with default settings", {
  object <- hermes_data
  control <- control_quality()
  result <- expect_silent(h_low_expression_flag(object, control))
  expect_logical(result)
  expect_equal(length(result), nrow(object))
})

test_that("h_low_expression_flag function works as expected with custom settings", {
  object <- hermes_data
  control <- control_quality(min_cpm = 5, min_cpm_prop = 0.5, min_corr = .1, min_depth = 3)
  result <- expect_silent(h_low_expression_flag(object, control))
  expect_logical(result)
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

test_that("h_low_expression_flag also works for an object containing only a single sample", {
  object <- hermes_data[, 2L]
  result <- expect_silent(h_low_expression_flag(object))
  expect_logical(result)
  expect_named(result, genes(object))
})

# h_low_depth_flag ----

test_that("h_low_depth_flag function works as expected with default settings", {
  object <- hermes_data
  control <- control_quality()
  result <- expect_silent(h_low_depth_flag(object, control))
  expect_logical(result)
  expect_equal(length(result), ncol(object))
})

test_that("h_low_depth_flag function works as expected with custom settings", {
  object <- hermes_data
  control <- control_quality(min_cpm = 5, min_cpm_prop = 0.5, min_corr = .1, min_depth = 3)
  result <- expect_silent(h_low_depth_flag(object, control))
  expect_logical(result)
  expect_equal(length(result), ncol(object))
})

test_that("h_low_depth_flag fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- expect_silent(HermesData(get_se()))
  cont1 <- control_quality()
  cont2 <- list(1, 2, 3, 4)
  expect_error(h_low_depth_flag(object1, cont1))
  expect_error(h_low_depth_flag(object3, cont2))
  expect_error(h_low_depth_flag(object2, cont1))
  expect_error(h_low_depth_flag(object2, cont2))
})

test_that("h_low_depth_flag also works for an object containing only a single gene", {
  object <- hermes_data[2L, ]
  result <- expect_silent(h_low_depth_flag(object))
  expect_logical(result)
  expect_named(result, samples(object))
})

# h_tech_failure_flag ----

test_that("h_tech_failure_flag function works as expected with default settings", {
  object <- hermes_data
  result <- expect_silent(h_tech_failure_flag(object))
  expect_logical(result)
  expect_equal(length(result), ncol(object))
})

test_that("h_tech_failure_flag function works as expected with custom settings", {
  object <- hermes_data
  control <- control_quality(min_corr = 0.35)
  result <- expect_silent(h_tech_failure_flag(object, control))
  expect_logical(result)
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

test_that("h_tech_failure_flag gives all FALSE for an object containing only a single gene", {
  object <- hermes_data[2L, ]
  result <- expect_silent(h_tech_failure_flag(object))
  expect_logical(result)
  expect_true(all(sapply(result, isFALSE)))
  expect_named(result, samples(object))
})

# get_tech_failure ----

test_that("get_tech_failure function works as expected", {
  object <- HermesData(get_se())
  result <- expect_silent(get_tech_failure(object))
  expected <- c(X = FALSE, Y = TRUE)
  expect_identical(result, expected)
})

test_that("get_tech_failure fails as expected with invalid input", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  expect_error(get_tech_failure(object1))
  expect_error(get_tech_failure(object2))
})

# get_low_depth ----

test_that("get_low_depth function works as expected", {
  object <- HermesData(get_se())
  result <- expect_silent(get_low_depth(object))
  expected <- c(X = FALSE, Y = FALSE)
  expect_identical(result, expected)
})

test_that("get_low_depth fails as expected with invalid input", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  expect_error(get_low_depth(object1))
  expect_error(get_low_depth(object2))
})

# get_low_expression ----

test_that("get_low_expression function works as expected", {
  object <- HermesData(get_se())
  result <- expect_silent(get_low_expression(object))
  expected <- c("GeneID:a" = TRUE, "GeneID:b" = FALSE)
  expect_identical(result, expected)
})

test_that("get_low_expression fails as expected with invalid input", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  expect_error(get_low_expression(object1))
  expect_error(get_low_expression(object2))
})

# set_tech_failure ----

test_that("set_tech_failure function works as expected", {
  object <- hermes_data
  ids <- c("06520063C0043R", "06520105C0017R", "06520092C0017R", "06520103C0017R")
  expect_false(any(get_tech_failure(object)[ids]))
  result <- expect_silent(set_tech_failure(object, ids))
  expect_true(all(get_tech_failure(result)[ids]))
})

test_that("set_tech_failure fails as expected with invalid inputs", {
  object1 <- get_se()
  object2 <- matrix(1:10, 2, 5)
  object3 <- expect_silent(HermesData(get_se()))
  sample_ids1 <- "0.5"
  sample_ids2 <- list(1, 2, 3, 4)
  expect_error(set_tech_failure(object1, sample_ids1))
  expect_error(set_tech_failure(object2, sample_ids2))
  expect_error(set_tech_failure(object3, sample_ids2))
  expect_error(set_tech_failure(object3, sample_ids1))
})
