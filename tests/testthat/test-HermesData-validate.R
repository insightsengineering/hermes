# validate_counts ----

test_that("validate_counts returns NULL for a valid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1:4, 2, 2))
  )
  expect_null(validate_counts(object))
})

test_that("validate_counts returns messages as expected for invalid objects", {
  object <- SummarizedExperiment(
    list(a = matrix(rnorm(4), 2, 2))
  )
  expect_identical(validate_counts(object), "no 'counts' assay found, consider using rename() to change assay name")

  object <- SummarizedExperiment(
    list(a = matrix(rnorm(4), 2, 2), counts = matrix(1:4, 2, 2))
  )
  expect_identical(validate_counts(object), "'counts' must be the first assay")

  object <- SummarizedExperiment(
    list(counts = matrix(c(1, 2, NA, -4), 2, 2))
  )
  expect_setequal(
    validate_counts(object),
    c(
      "'counts' must be numeric in integer mode", "missing values in 'counts'",
      "negative values in 'counts'"
    )
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

# validate_row_data ----

test_that("validate_row_data returns NULL for a valid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowData = data.frame(
      symbol = 1,
      chromosome = 1,
      size = 1,
      desc = 1,
      low_expression_flag = 1
    )
  )
  expect_null(validate_row_data(object))
})

test_that("validate_row_data returns messages as expected for invalid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_match(
    validate_row_data(object),
    "required columns .+ not present"
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    rowData = data.frame(
      symbol = 1,
      chromosome = 1,
      size = 1,
      desc = 1
    )
  )
  expect_identical(
    validate_row_data(object),
    "required columns low_expression_flag not present"
  )
})

# validate_col_data ----

test_that("validate_col_data returns NULL for a valid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    colData = data.frame(
      low_depth_flag = 1,
      tech_failure_flag = 1
    )
  )
  expect_null(validate_col_data(object))
})

test_that("validate_col_data returns messages as expected for invalid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_match(
    validate_col_data(object),
    "required columns .+ not present"
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1)),
    colData = data.frame(
      tech_failure_flag = 1
    )
  )
  expect_identical(
    validate_col_data(object),
    "required columns low_depth_flag not present"
  )
})

# validate_names ----

test_that("validate_names returns NULL for a valid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(
      data = 1:4,
      nrow = 2,
      ncol = 2,
      dimnames = list(c("a", "b"), c("X", "Y"))
    ))
  )
  expect_null(validate_names(object))
})

test_that("validate_names returns messages as expected for invalid object", {
  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1))
  )
  expect_identical(
    validate_names(object),
    c("'object' must have rownames", "'object' must have colnames")
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1, dimnames = list(NULL, "bla")))
  )
  expect_identical(
    validate_names(object),
    "'object' must have rownames"
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 1, 1, dimnames = list("bla", NULL)))
  )
  expect_identical(
    validate_names(object),
    "'object' must have colnames"
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 2, 2, dimnames = list(c("a", "a"), c("x", "y"))))
  )
  expect_identical(
    validate_names(object),
    "'object' must have unique rownames"
  )

  object <- SummarizedExperiment(
    list(counts = matrix(1L, 2, 2, dimnames = list(c("a", "b"), c("x", "x"))))
  )
  expect_identical(
    validate_names(object),
    "'object' must have unique colnames"
  )
})

# validate_prefix  ----

test_that("validate_prefix returns NULL for a valid object", {
  object <- hermes_data
  expect_null(validate_prefix(object))
})

test_that("validate_prefix returns messages as expected for wrong prefix with whitespace", {
  object <- hermes_data
  object@prefix <- "Gene ID"
  result <- validate_prefix(object)
  expected <- c(
    "'prefix' can only consist of alphabetic characters",
    "'prefix' does not match at least one gene ID"
  )
  expect_identical(result, expected)
})

test_that("validate_prefix returns correct message when prefix has two elements", {
  object <- hermes_data
  object@prefix <- c("GeneID", "ENSGID")
  result <- expect_silent(validate_prefix(object))
  expected <- "'prefix' must be string"
  expect_identical(result, expected)
})
