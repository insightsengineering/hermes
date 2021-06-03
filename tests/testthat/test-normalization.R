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

# h_tpm ----

test_that("h_tpm function works as expected with default settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- h_tpm(object, cont)
  expect_is(result, "matrix")
})

test_that("h_tpm function works as expected with custom settings", {
  object <- HermesData(get_se())
  cont <- expect_silent(control_normalize(log = TRUE, lib_sizes = 140000000L, prior_count = 7))
  result <- h_tpm(object, cont)
  expect_is(result, "matrix")
})

test_that("h_tpm function fails as expected with invalid settings", {
  object <- get_se()
  expect_error(h_tpm(object, cont))
})