# .HermesData ----

test_that("HermesData objects can be created with default constructor .HermesData", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowData = data.frame(
      HGNC = 1, 
      GeneID = 1, 
      Chromosome = 1, 
      StartBP = 1, 
      EndBP = 1, 
      WidthBP = 1, 
      HGNCGeneName = 1, 
      CanonicalTranscript = 1, 
      ProteinTranscript = 1, 
      LowExpressionFlag = 1
    ),
    colData = data.frame(
      SampleID = 1, 
      LowDepthFlag = 1, 
      TechnicalFailureFlag = 1
    )
  )
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

# HermesData ----

test_that("HermesData objects can be created with constructor HermesData", {
  result <- expect_silent(HermesData(summarized_experiment))
  expect_is(result, "HermesData")
})

test_that("HermesData constructor fails with readable error message when there are no assays", {
  input <- SummarizedExperiment()
  expect_error(HermesData(input), "assays(object) has an empty dimension")
})

# makeSummarizedExperimentFromExpressionSet ----

test_that("SummarizedExperiment can be created from ExpressionSet", {
  object <- expression_set
  result <- expect_silent(makeSummarizedExperimentFromExpressionSet(object))
  expect_is(result, "SummarizedExperiment")
  expect_true(validObject(result))
})
