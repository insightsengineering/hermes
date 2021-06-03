# control_normalize ----

test_that("control_normalize function works as expected with default settings", {
  result <- control_normalize()
  expect_is(result, "list")
  expect_named(result, c("log", "lib_sizes", "prior_count"))
})

test_that("control_normalize function works as expected with custom settings", {
  result <- expect_silent(control_normalize(log = TRUE, lib_sizes = 60000000L, prior_count = 3))
  expect_is(result, "list")
  expect_identical(result$log, TRUE)
  expect_identical(result$lib_sizes, 60000000L)
  expect_identical(result$prior_count, 3)
})

test_that("control_normalize fails as expected with invalid settings", {
  expect_error(control_normalize(log = "TRUE"))
  expect_error(control_normalize(lib_sizes = 1))
  expect_error(control_normalize(lib_sizes = -1L))
  expect_error(control_normalize(lib_sizes = 0L))
  expect_error(control_normalize(prior_count = -1))
})

# h_rpkm ----

test_that("h_rpkm function works as expected with default settings", {
  result <- h_rpkm()
  expect_is(result, "matrix")
})