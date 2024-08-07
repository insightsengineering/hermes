# .HermesData ----

test_that("HermesData objects can be created with default constructor .HermesData", {
  object <- get_se()
  result <- expect_silent(.HermesData(object, prefix = "GeneID"))
  expect_s4_class(result, "HermesData")
  expect_true(validObject(result))
})

test_that("HermesData validation fails as expected", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_error(.HermesData(object), "required columns .+ not present")
})

test_that("HermesData prefix slot can not be assigned numeric", {
  object <- hermes_data
  expect_error(object@prefix <- 124)
})

# .RangedHermesData ----

test_that("RangedHermesData objects can be created with default constructor .RangedHermesData", {
  object <- get_rse()
  result <- expect_silent(.RangedHermesData(object, prefix = "ENSG"))
  expect_s4_class(result, "RangedHermesData")
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
  expect_s4_class(result, "SummarizedExperiment")
  expect_true(validObject(result))
})

# HermesData ----

test_that("HermesData objects can be created with constructor HermesData", {
  result <- expect_silent(hermes_data)
  expect_s4_class(result, "HermesData")
})

test_that("HermesData constructor fails with readable error message when there are no assays", {
  input <- SummarizedExperiment()
  expect_error(HermesData(input), "assays(object) has an empty dimension", fixed = TRUE)
})

test_that("HermesData determines prefix correctly", {
  result <- expect_silent(HermesData(get_se()))
  expect_identical(result@prefix, "GeneID")
  result <- expect_silent(HermesData(get_rse()))
  expect_identical(result@prefix, "ENSG")
})

test_that("HermesData accepts SummarizedExperiment object with rowData or colData", {
  object <- get_se()
  colData(object) <- NULL
  rowData(object) <- NULL
  result <- expect_silent(HermesData(object))
  expect_true(all(.col_data_cols %in% names(colData(result))))
  expect_true(all(.row_data_cols %in% names(rowData(result))))
  expect_true(all(is.na(colData(result))))
  expect_true(all(is.na(rowData(result))))
})

test_that("HermesData creates missing columns with NAs correctly", {
  object <- get_se()
  colData(object) <- colData(object)[, "low_depth_flag", drop = FALSE]
  rowData(object) <- rowData(object)[, c("symbol", "size")]
  expect_false(all(.col_data_cols %in% names(colData(object))))
  expect_false(all(.row_data_cols %in% names(rowData(object))))
  result <- expect_silent(HermesData(object))
  expect_true(all(.col_data_cols %in% names(colData(result))))
  expect_true(all(.row_data_cols %in% names(rowData(result))))
})

test_that("HermesData converts DelayedMatrix assays correctly to matrix assays", {
  se <- summarized_experiment
  assay(se, "delayed") <- DelayedArray::DelayedArray(assay(se, "counts"))
  expect_s4_class(assay(se, "delayed"), "DelayedMatrix")
  result <- HermesData(se)
  expect_matrix(assay(result, "delayed"))
  a1 <- assay(se, "delayed")
  a2 <- assay(result, "delayed")
  expect_identical(dim(a1), dim(a2))
  expect_identical(as.integer(a1), as.integer(a2))
})

test_that("RangedHermesData objects can be created with constructor HermesData", {
  result <- expect_silent(HermesData(get_rse()))
  expect_s4_class(result, "RangedHermesData")
})

# HermesDataFromMatrix ----

test_that("HermesData objects can be created with constructor HermesDataFromMatrix", {
  counts <- assay(summarized_experiment)
  result <- expect_silent(HermesDataFromMatrix(
    counts = counts,
    rowData = rowData(summarized_experiment),
    colData = colData(summarized_experiment)
  ))
  expect_s4_class(result, "HermesData")
})

test_that("HermesDataFromMatrix also works when just passing the count matrix", {
  counts <- assay(summarized_experiment)
  result <- expect_silent(HermesDataFromMatrix(counts))
  expect_s4_class(result, "HermesData")
})

test_that("RangedHermesData objects can be created with constructor HermesDataFromMatrix", {
  rse <- get_rse()
  counts <- assay(rse)
  result <- expect_silent(HermesDataFromMatrix(
    counts = counts,
    rowRanges = rowRanges(rse),
    colData = colData(rse)
  ))
  expect_s4_class(result, "RangedHermesData")
})
