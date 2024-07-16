# control_normalize ----

test_that("control_normalize function works as expected with default settings", {
  result <- control_normalize()
  expect_list(result, len = 4)
  expect_named(result, c("log", "lib_sizes", "prior_count", "fit_type"))
})

test_that("control_normalize function works as expected with custom settings", {
  result <- expect_silent(control_normalize(log = TRUE, lib_sizes = 60000000L, prior_count = 3, fit_type = "mean"))
  expect_list(result, len = 4)
  expect_identical(result$log, TRUE)
  expect_identical(result$lib_sizes, 60000000L)
  expect_identical(result$prior_count, 3)
  expect_identical(result$fit_type, "mean")
})

test_that("control_normalize fails as expected with invalid settings", {
  expect_error(control_normalize(log = "TRUE"))
  expect_error(control_normalize(lib_sizes = 1))
  expect_error(control_normalize(lib_sizes = -1L))
  expect_error(control_normalize(lib_sizes = 0L))
  expect_error(control_normalize(prior_count = -1))
})

# h_cpm ----

test_that("h_cpm function works as expected with default settings", {
  object <- hermes_data
  cont <- control_normalize()
  result <- expect_silent(h_cpm(object, cont))
  expect_matrix(result)
  expect_equal(dim(result), dim(object))
})

test_that("t_cpm function works as expected with custom settings", {
  object <- HermesData(get_se())
  cont <- expect_silent(control_normalize(log = TRUE, lib_sizes = 60000000L, prior_count = 3))
  result <- expect_silent(h_cpm(object, cont))
  expect_matrix(result)
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

# h_rpkm ----

test_that("h_rpkm function works as expected with default settings", {
  object <- hermes_data
  cont <- control_normalize()
  result <- h_rpkm(object, cont)
  expect_matrix(result)
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

test_that("h_rpkm function fails when at least one gene has missing `size`", {
  object <- hermes_data
  cont <- control_normalize()
  rowData(object)$size[1] <- NA # nolint
  expect_error(
    h_rpkm(object, cont),
    "rowData(object)$size contains 1 missing values",
    fixed = TRUE
  )
})

# h_tpm ----

test_that("h_tpm function works as expected with default settings", {
  object <- hermes_data
  cont <- control_normalize()
  result <- h_tpm(object, cont)
  expect_matrix(result)
})

test_that("h_tpm function works as expected with custom settings", {
  object <- HermesData(get_se())
  cont <- expect_silent(control_normalize(log = TRUE, lib_sizes = 140000000L, prior_count = 7))
  result <- h_tpm(object, cont)
  expect_matrix(result)
})

test_that("h_tpm function fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- HermesData(get_se())
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_tpm(object1, cont1))
  expect_error(h_tpm(object3, cont2))
  expect_error(h_tpm(object2, cont1))
  expect_error(h_tpm(object2, cont2))
})

# h_voom ----

test_that("h_voom function works as expected with default settings", {
  object <- hermes_data
  result <- expect_silent(h_voom(object))
  expect_matrix(result)
})

test_that("h_voom function works as expected with custom settings", {
  object <- hermes_data
  cont <- control_normalize(log = TRUE, lib_sizes = 1000000L, prior_count = 10)
  result <- expect_silent(h_voom(object, cont))
  expect_matrix(result)
})

test_that("h_voom fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- HermesData(get_se())
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_voom(object1, cont1))
  expect_error(h_voom(object3, cont2))
  expect_error(h_voom(object2, cont1))
  expect_error(h_voom(object2, cont2))
})

test_that("h_voom works when there are no samples", {
  object <- HermesData(get_se())[, -c(1, 2)]
  assert_that(identical(ncol(object), 0L))
  result <- expect_silent(h_voom(object))
  expect_identical(result, counts(object))
})

test_that("h_voom works when there are no genes", {
  object <- HermesData(get_se())[-c(1, 2), ]
  assert_that(identical(nrow(object), 0L))
  result <- expect_silent(h_voom(object))
  expect_identical(result, counts(object))
})

# normalize ----

test_that("normalize works as expected for HermesData", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(normalize(h1, c("cpm", "rpkm", "tpm", "voom")))
  expect_s4_class(result, "HermesData")
  expect_named(assays(result), c("counts", "cpm", "rpkm", "tpm", "voom"))
})

test_that("normalize works as expected for RangedHermesData", {
  object <- get_rse()
  h1 <- HermesData(object)
  result <- expect_silent(normalize(h1, c("cpm", "rpkm", "tpm", "voom")))
  expect_s4_class(result, "RangedHermesData")
  expect_named(assays(result), c("counts", "cpm", "rpkm", "tpm", "voom"))
})

test_that("normalize fails as expected with wrong method choice", {
  object <- get_rse()
  h1 <- HermesData(object)
  expect_error(normalize(h1, method = "bla"))
  expect_error(normalize(h1, method = c("cpm", "bla")))
})

test_that("normalize works when global environment overwrites helper function", {
  object <- get_rse()
  h1 <- HermesData(object)
  h_cpm <- function(object, control) {
    stop("wrong helper function was used")
  }
  expect_silent(normalize(h1, method = "cpm"))
})

# h_vst ----

test_that("h_vst function works as expected with default settings", {
  object <- hermes_data
  result <- expect_silent(h_vst(object))
  expect_matrix(result)
})

test_that("h_vst function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  cont <- control_normalize(fit_type = "mean")
  result <- expect_silent(h_vst(object, cont))
  expect_matrix(result)
})

test_that("h_vst fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- HermesData(get_se())
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_vst(object1, cont1))
  expect_error(h_vst(object3, cont2))
  expect_error(h_vst(object2, cont1))
  expect_error(h_vst(object2, cont2))
})

test_that("h_vst fails when there are no samples", {
  object <- HermesData(get_se())[, -c(1, 2)]
  assert_that(identical(ncol(object), 0L))
  expect_error(h_vst(object))
})

test_that("h_vst fails when there are no genes", {
  object <- HermesData(get_se())[-c(1, 2), ]
  assert_that(identical(nrow(object), 0L))
  expect_error(h_vst(object))
})

test_that("h_vst gives understandable error for an object with few genes", {
  object <- HermesData(get_se())
  expect_error(
    h_vst(object),
    "try again with more genes"
  )
})

# h_rlog ----

test_that("h_rlog function works as expected with default settings", {
  object <- hermes_data
  result <- expect_silent(h_rlog(object))
  expect_matrix(result)
})

test_that("h_rlog function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  cont <- control_normalize(fit_type = "mean")
  result <- expect_silent(h_rlog(object, cont))
  expect_matrix(result)
})

test_that("h_rlog fails as expected with invalid settings", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  object3 <- HermesData(get_se())
  cont1 <- control_normalize()
  cont2 <- list(1, 2, 3)
  expect_error(h_rlog(object1, cont1))
  expect_error(h_rlog(object3, cont2))
  expect_error(h_rlog(object2, cont1))
  expect_error(h_rlog(object2, cont2))
})

test_that("h_rlog fails when there are no samples", {
  object <- HermesData(get_se())[, -c(1, 2)]
  assert_that(identical(ncol(object), 0L))
  expect_error(h_rlog(object))
})

test_that("h_rlog fails when there are no genes", {
  object <- HermesData(get_se())[-c(1, 2), ]
  assert_that(identical(nrow(object), 0L))
  expect_error(h_rlog(object))
})
