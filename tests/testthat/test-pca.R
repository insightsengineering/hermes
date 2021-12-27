# calc_pca ----

test_that("calc_pca function works as expected for HermesData with default counts assay", {
  object <- hermes_data
  result <- expect_silent(normalize(object))
  pca <- expect_silent(calc_pca(result))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function works as expected for HermesData with another assay", {
  object <- hermes_data
  result <- expect_silent(normalize(object))
  pca <- expect_silent(calc_pca(result, assay_name = "cpm"))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function works for RangedHermesData with only 2 samples and cpm/rpkm/tpm/voom normalization", {
  object <- HermesData(get_rse())
  expect_is(object, "RangedHermesData")
  result <- expect_silent(normalize(object, c("cpm", "rpkm", "tpm", "voom")))
  pca <- expect_silent(calc_pca(result))
  expect_is(pca, "HermesDataPca")
  expect_named(pca, c("sdev", "rotation", "center", "scale", "x"))
})

test_that("calc_pca function fails for RangedHermesData with only 2 samples and vst/rlog transformation", {
  object <- HermesData(get_rse())
  expect_is(object, "RangedHermesData")
  result <- expect_error(normalize(object, c("vst", "rlog")))
  expect_error(calc_pca(result))
})

test_that("calc_pca function fails as expected with wrong assay choice", {
  object <- hermes_data
  result <- expect_silent(normalize(object))
  expect_error(calc_pca(result, assay_name = "abc"))
  expect_error(calc_pca(result, assay_name = c("counts", "zyz")))
})

test_that("calc_pca function works as expected with n_top option", {
  object <- hermes_data
  pca <- expect_silent(calc_pca(object, n_top = 500))
  expect_identical(nrow(pca$rotation), 500L)
})

test_that("calc_pca function returns less genes than specified in n_top", {
  object <- hermes_data
  pca <- expect_silent(calc_pca(object, n_top = 10000))
  expect_true(nrow(pca$rotation) < 10000)
})
