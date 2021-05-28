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
