# Create HermesData ----


# # Create SE for testing ---- 
# nrows1 <- 200; ncols1 <- 6
# counts1 <- matrix(runif(nrows1 * ncols1, 1, 1e4), nrows1)
# colData1 <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#                       row.names=LETTERS[1:6])
# se0 <- SummarizedExperiment(assays=SimpleList(counts=counts1),
#                             colData=colData1)
# se1 <- SummarizedExperiment(assays=SimpleList(counts=counts1),
#                             colData=colData1)
# 
# # Create a matrix for testing ----
# matrix1 <- matrix(1L, 2, 2)
# matrix2 <- matrix(2L, 2, 2)

# rbind ----
test_that("Test rbind function",{
  object <- SummarizedExperiment::SummarizedExperiment(
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
  h1 <- .HermesData(object[1])
  h2 <- .HermesData(object[2])
  h3 <- .HermesData(object)
  expect_equal(rbind(h1, h2), h3)
  expect_equal(dim(rbind(h1, h2)), dim(h3))
  expect_equal(rowData(rbind(h1, h2)), rowData(h3))
  expect_equal(colData(rbind(h1, h2)), colData(h3))
})

# test_that("rbind function fails for non-HermesData objects", {
#   expect_error(rbind(se0, se1), "Not all input objects are HermesData, please check again")
#   expect_error(rbind(matrix1, matrix2), "Not all input objects are HermesData, please check again")
# })
