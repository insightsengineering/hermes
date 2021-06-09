# calc_pca ----

test_that("calc_pca function works as expected for HermesData with default counts assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  pca <- expect_silent(calc_pca(result))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function works as expected for HermesData with another assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  pca <- expect_silent(calc_pca(result, assay_name = "cpm"))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  expect_is(object, "RangedHermesData")
  result <- expect_silent(normalize(object))
  pca <- expect_silent(calc_pca(result))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function fails as expected with wrong assay choice", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  expect_error(calc_pca(result, assay_name = "abc"))
  expect_error(calc_pca(result, assay_name = c("counts", "zyz")))
})

# calc_cor ----

test_that("calc_cor function works as expected with default settings", {
  object <- HermesData(summarized_experiment)
  result <- expect_silent(calc_cor(object))
  expect_s4_class(result, "HermesDataCor")
  expect_named(result@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
})

test_that("calc_cor function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  result1 <- expect_silent(calc_cor(object, method = "spearman"))
  expect_s4_class(result1, "HermesDataCor")
  expect_named(result1@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
  
  result2 <- calc_cor(object)
  expect_false(identical(result1, result2))
})

test_that("calc_cor fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  se <- get_se()
  expect_error(calc_cor(se))
  expect_error(calc_cor(object, method = "foo"))
  expect_error(calc_cor(object, assay_name = 1))
})
