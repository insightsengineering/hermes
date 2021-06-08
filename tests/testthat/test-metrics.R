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

# cor ----

test_that("cor function works as expected with default settings", {
  object <- HermesData(summarized_experiment)
  result <- expect_silent(cor(x = object))
  expect_is(result, "HermesDataCor")
  expect_s4_class(result, "HermesDataCor")
  expect_named(result@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
})

test_that("cor function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  result1 <- expect_silent(cor(x = object, method = "spearman"))
  expect_is(result1, "HermesDataCor")
  expect_s4_class(result1, "HermesDataCor")
  expect_named(result1@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
  
  result2 <- cor(x = object)
  expect_false(identical(result1, result2))
})

test_that("cor fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  gSE_object <- get_se()
  expect_error(cor(x = gSE_object))
  expect_error(cor(x = object, method = "foo"))
  expect_error(cor(x = object, y = "tallys"))
})
