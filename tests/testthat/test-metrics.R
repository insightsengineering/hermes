# calc_pca ----
test_that("calc_pca function works as expected for HermesData", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  expect_is(result, "HermesData")
  expect_named(assays(result), c("counts", "cpm", "rpkm", "tpm", "voom"))
  pca1 <- expect_silent(calc_pca(result))
  pca2 <- expect_silent(calc_pca(result, assay_name = "cpm"))
  expect_is(pca1, "HermesDataPca")
  expect_is(pca2, "HermesDataPca")
  expect_vector(names(pca1), c("sdev", "rotation", "center", "scale", "x"))
  expect_vector(names(pca2), c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  result <- expect_silent(normalize(object))
  expect_is(result, "RangedHermesData")
  expect_named(assays(result), c("counts", "cpm", "rpkm", "tpm", "voom"))
  pca1 <- expect_silent(calc_pca(result))
  pca2 <- expect_silent(calc_pca(result, assay_name = "cpm"))
  expect_is(pca1, "HermesDataPca")
  expect_is(pca2, "HermesDataPca")
  expect_vector(names(pca1), c("sdev", "rotation", "center", "scale", "x"))
  expect_vector(names(pca2), c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function fails as expected with wrong assay choice", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  expect_error(calc_pca(result, assay_name = "abc"))
  expect_error(calc_pca(result, assay_name = c("counts", "zyz")))
})