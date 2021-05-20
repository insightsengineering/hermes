get_se <- function() {
  SummarizedExperiment::SummarizedExperiment(
    list(counts = matrix(1L, 2, 1)),
    rowData = data.frame(
      HGNC = c(1, 1),
      GeneID = c(1, 1),
      Chromosome = c(1, 1),
      StartBP = c(1, 1),
      EndBP = c(1, 1),
      WidthBP = c(1, 1),
      HGNCGeneName = c(1, 1),
      CanonicalTranscript = c(1, 1),
      ProteinTranscript = c(1, 1),
      LowExpressionFlag = c(1, 1)
    ),
    colData = data.frame(
      SampleID = 1,
      LowDepthFlag = 1,
      TechnicalFailureFlag = 1
    ),
    metadata = list(
      filename = "bla.txt",
      hash = "9352983502"
    )
  )
}

# rbind ----

test_that("rbind function works as expected for HermesData objects", {
  object <- get_se()
  h1 <- .HermesData(object[1])
  h2 <- .HermesData(object[2])
  h3 <- .HermesData(object)
  result <- expect_silent(rbind(h1, h2))
  expect_is(result, "HermesData")
  expect_equal(dim(result), dim(h3))
  expect_equal(rowData(result), rowData(h3))
  expect_equal(colData(result), colData(h3))
})

test_that("rbind function works as expected when binding SummarizedExperiment with HermesData", {
  object <- get_se()
  h1 <- .HermesData(object)
  result1 <- expect_silent(rbind(object, h1))
  expect_is(result1, "SummarizedExperiment")
  result2 <- expect_silent(rbind(h1, object))
  expect_is(result2, "SummarizedExperiment")
})

# metadata ----

test_that("metadata accessor works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  result <- expect_silent(metadata(h1))
  expected <- list(
    filename = "bla.txt",
    hash = "9352983502"
  )
  expect_identical(result, expected)
})

test_that("metadata setter works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  value <- list(a = "foo")
  expect_silent(metadata(h1) <- value)
  expect_identical(metadata(h1), value)
})

# counts ----

test_that("counts accessor works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  result <- expect_silent(counts(h1))
  expect_is(result, "matrix")
  expect_identical(dim(result), dim(h1))
})

test_that("counts setter works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  value <- matrix(0L, nrow = nrow(h1), ncol = ncol(h1))
  expect_silent(counts(h1) <- value)
  expect_equivalent(counts(h1), value)
})
