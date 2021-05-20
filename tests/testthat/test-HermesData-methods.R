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
    )
  )
}

# rbind ----
test_that("rbind function works as expected for HermesData objects",{
  object <- get_se()
  h1 <- .HermesData(object[1])
  h2 <- .HermesData(object[2])
  h3 <- .HermesData(object)
  expect_equal(rbind(h1, h2), h3)
  expect_equal(dim(rbind(h1, h2)), dim(h3))
  expect_equal(rowData(rbind(h1, h2)), rowData(h3))
  expect_equal(colData(rbind(h1, h2)), colData(h3))
})

test_that("rbind function gives meaningful error message for non-HermesData objects", {
  object <- get_se()
  h1 <- .HermesData(object)
  expect_error(rbind(object, h1), "one argument is not of class HermesData, please first convert it")
  expect_error(rbind(h1, object), "one argument is not of class HermesData, please first convert it")
})
