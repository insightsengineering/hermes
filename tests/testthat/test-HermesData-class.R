# .HermesData ----

test_that("HermesData objects can be created with default constructor .HermesData", {
  object <- get_se()
  result <- expect_silent(.HermesData(object))
  expect_is(result, "HermesData")
  expect_true(validObject(result))
})

test_that("HermesData validation fails as expected", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_error(.HermesData(object), "required columns .+ not present")
})

# .RangedHermesData ----

test_that("RangedHermesData objects can be created with default constructor .RangedHermesData", {
  object <- get_rse()
  result <- expect_silent(.RangedHermesData(object))
  expect_is(result, "RangedHermesData")
  expect_true(validObject(result))
})

test_that("RangedHermesData validation fails as expected", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowRanges = GRanges(
      "chr1",
      IRanges(124L, width = 100),
      strand = "+",
      feature_id = 1L
    )
  )
  expect_error(.RangedHermesData(object), "required columns .+ not present")
})

# makeSummarizedExperimentFromExpressionSet ----

test_that("SummarizedExperiment can be created from ExpressionSet", {
  object <- expression_set
  result <- expect_silent(makeSummarizedExperimentFromExpressionSet(object))
  expect_is(result, "SummarizedExperiment")
  expect_true(validObject(result))
})

# HermesData ----

test_that("HermesData objects can be created with constructor HermesData", {
  result <- expect_silent(HermesData(summarized_experiment))
  expect_is(result, "HermesData")
})

test_that("HermesData constructor fails with readable error message when there are no assays", {
  input <- SummarizedExperiment()
  expect_error(HermesData(input), "assays(object) has an empty dimension", fixed = TRUE)
})

test_that("RangedHermesData objects can be created with constructor HermesData", {
  result <- expect_silent(HermesData(get_rse()))
  expect_is(result, "RangedHermesData")
})

# HermesDataFromMatrix ----

test_that("HermesData objects can be created with constructor HermesDataFromMatrix", {
  counts <- assay(summarized_experiment)
  result <- expect_silent(HermesDataFromMatrix(
    counts = counts,
    rowData = rowData(summarized_experiment),
    colData = colData(summarized_experiment)
  ))
  expect_is(result, "HermesData")
})

test_that("RangedHermesData objects can be created with constructor HermesDataFromMatrix", {
  rse <- get_rse()
  counts <- assay(rse)
  result <- expect_silent(HermesDataFromMatrix(
    counts = counts,
    rowRanges = rowRanges(rse),
    colData = colData(rse)
  ))
  expect_is(result, "RangedHermesData")
})
