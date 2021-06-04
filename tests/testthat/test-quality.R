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
