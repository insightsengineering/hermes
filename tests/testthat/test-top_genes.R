# top_genes ----

test_that("top_genes function works as expected for HermesData with default counts assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(top_genes(object))
  expect_s4_class(result, "HermesDataTopGenes")
  expect_named(result, c("expression", "name"))
  expect_identical(result@summary_fun_name, "rowMeans")
  expect_identical(result@assay_name, "counts")
})

test_that("top_genes function works as expected for HermesData with another assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  object_norm <- expect_silent(normalize(object))
  result <- expect_silent(top_genes(object_norm, assay_name = "cpm"))
  expect_s4_class(result, "HermesDataTopGenes")
})

test_that("top_genes function works as expected for RangedHermesData", {
  object <- expect_silent(HermesData(get_rse()))
  result <- expect_silent(top_genes(object))
  expect_s4_class(result, "HermesDataTopGenes")
  expect_named(result, c("expression", "name"))
  expect_identical(result@summary_fun_name, "rowMeans")
  expect_identical(result@assay_name, "counts")
})

test_that("top_genes function fails as expected with wrong assay choice", {
  object <- expect_silent(normalize(HermesData(summarized_experiment)))
  expect_error(top_genes(object, assay_name = 1))
  expect_error(top_genes(object, assay_name = c("counts", "cpm")))
})

test_that("top_genes function fails as expected with wrong summary function", {
  object <- expect_silent(normalize(HermesData(summarized_experiment)))
  expect_error(top_genes(object, summary_fun = "rowMeans"))
  expect_error(top_genes(object, summary_fun = sum))
  expect_error(top_genes(object, summary_fun = colMeans))
})

test_that("top_genes function fails as expected with when selection criteria are incorrect", {
  object <- expect_silent(HermesData(summarized_experiment))
  expect_error(top_genes(object, n_top = 10L, min_threshold = 10))
  expect_error(top_genes(object, n_top = NULL, min_threshold = NULL))
  expect_error(top_genes(object, n_top = 0))
  expect_error(top_genes(object, min_threshold = 0))
  expect_error(top_genes(object, min_threshold = Inf))
  expect_error(top_genes(object, min_threshold = -1))
})

test_that("top_genes function works as expected with correct selection criteria", {
  object <- expect_silent(HermesData(summarized_experiment))
  
  result1 <- expect_silent(top_genes(object, n_top = 5L))
  expect_identical(nrow(result1), 5L)
  
  result2 <- expect_silent(top_genes(object, min_threshold = 200))
  expect_true(min(result2$expression) > 200)
  expect_false(identical(result1, result2))
})
