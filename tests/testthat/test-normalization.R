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
  object <- expect_silent(HermesData(summarized_experiment))
  cont <- control_normalize()
  result <- h_rpkm(object, cont)
  expect_is(result, "matrix")
  expect_equal(dim(object), dim(result))
})

test_that("h_rpkm function works as expected with custom settings", {
  object <- HermesData(get_se())
  cont <- expect_silent(control_normalize(log = TRUE, lib_sizes = 180000000L, prior_count = 5))
  result <- h_rpkm(object, cont)
  expect_gt(min(result), 0)
  expect_equal(dim(object), dim(result))
})

test_that("h_rpkm function fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- HermesData(get_se())
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_rpkm(object1, cont1))
  expect_error(h_rpkm(object3, cont2))
  expect_error(h_rpkm(object2, cont1))
  expect_error(h_rpkm(object2, cont2))
})
# h_cpm ----

test_that("h_cpm function works as expected with default settings", {
  object <- expect_silent(HermesData(summarized_experiment))
  cont <- control_normalize()
  result <- expect_silent(h_cpm(object, cont))
  expect_is(result, "matrix")
  expect_equal(dim(result), dim(object))
})

test_that("t_cpm function works as expected with custom settings", {
  object <- HermesData(get_se())
  cont <- expect_silent(control_normalize(log = TRUE, lib_sizes = 60000000L, prior_count = 3))
  result <- expect_silent(h_cpm(object,cont))
  expect_is(result, "matrix")
  expect_equal(dim(result), dim(object))
})

test_that("h_cpm function fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- expect_silent(HermesData(get_se()))
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_cpm(object1, cont1))
  expect_error(h_cpm(object3, cont2))
  expect_error(h_cpm(object2, cont1))
  expect_error(h_cpm(object2, cont2))
})
