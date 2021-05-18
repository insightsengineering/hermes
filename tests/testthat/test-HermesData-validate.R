# validate_counts ----

test_that("validate_counts returns NULL for a valid object", {
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1:4, 2, 2))
  )
  expect_null(validate_counts(object))
})

test_that("validate_counts returns messages as expected for invalid objects", {
  object <- SummarizedExperiment::SummarizedExperiment(
    list(a = matrix(rnorm(4), 2, 2))
  )
  expect_identical(validate_counts(object), "no 'counts' assay found")
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(a = matrix(rnorm(4), 2, 2), counts = matrix(1:4, 2, 2))
  )
  expect_identical(validate_counts(object), "'counts' must be the first assay")
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(c(1, 2, NA, -4), 2, 2))
  )
  expect_setequal(
    validate_counts(object),
    c("'counts' must be numeric in integer mode", "missing values in 'counts'", 
      "negative values in 'counts'")
  )
})

# validate_cols ----

test_that("validate_cols returns NULL when columns are found", {
  expect_null(validate_cols(c("a", "b"), c("c", "b", "a", "d")))
})

test_that("validate_cols returns messages as expected when columns are not found", {
  expect_identical(
    validate_cols(c("a", "b"), c("c", "d", "b")), 
    "required columns a not present"
  )
  expect_identical(
    validate_cols(c("a", "b"), c("c", "d")), 
    "required columns a, b not present"
  )
})

# validate_non_empty ----

test_that("validate_non_empty returns NULL when each column in data frame is not empty", {
  expect_null(validate_non_empty(data.frame(a = c(NA, NA, 3), b = c(NA, "bla", NA))))
})

test_that("validate_non_empty returns messages as expected when some columns are empty", {
  expect_identical(
    validate_non_empty(data.frame(a = c(NA, NA, 3), b = c(NA, NA, NA))),
    "columns b only contain NAs"
  )
  expect_identical(
    validate_non_empty(data.frame(a = c(NA, NA, NA), b = c(NA, NA, NA))),
    "columns a, b only contain NAs"
  )
})

# validate_row_data ----

test_that("validate_row_data returns NULL for a valid object", {
  object <- SummarizedExperiment::SummarizedExperiment(
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
    )
  )
  expect_null(validate_row_data(object))
})

test_that("validate_row_data returns messages as expected for invalid object", {
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_match(
    validate_row_data(object),
    "required columns .+ not present"
  )
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowData = data.frame(
      HGNC = 1, 
      GeneID = 1, 
      Chromosome = 1, 
      StartBP = 1, 
      EndBP = 1, 
      WidthBP = 1, 
      HGNCGeneName = 1, 
      CanonicalTranscript = 1
    )
  )
  expect_identical(
    validate_row_data(object),
    "required columns ProteinTranscript, LowExpressionFlag not present"
  )
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowData = data.frame(
      HGNC = NA, 
      GeneID = NA, 
      Chromosome = 1, 
      StartBP = 1, 
      EndBP = 1, 
      WidthBP = 1, 
      HGNCGeneName = 1, 
      CanonicalTranscript = 1, 
      ProteinTranscript = 1, 
      LowExpressionFlag = NA
    )
  )
  expect_identical(
    validate_row_data(object),
    "columns HGNC, GeneID only contain NAs"
  )
})

# validate_col_data ----

test_that("validate_col_data returns NULL for a valid object", {
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    colData = data.frame(
      SampleID = 1, 
      LowDepthFlag = 1, 
      TechnicalFailureFlag = 1
    )
  )
  expect_null(validate_col_data(object))
})

test_that("validate_col_data returns messages as expected for invalid object", {
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_match(
    validate_col_data(object),
    "required columns .+ not present"
  )
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    colData = data.frame(
      TechnicalFailureFlag = 1
    )
  )
  expect_identical(
    validate_col_data(object),
    "required columns SampleID, LowDepthFlag not present"
  )
  
  object <- SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    colData = data.frame(
      SampleID = NA, 
      LowDepthFlag = 1, 
      TechnicalFailureFlag = NA
    )
  )
  expect_identical(
    validate_col_data(object),
    "columns SampleID only contain NAs"
  )
})
