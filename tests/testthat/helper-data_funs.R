# get_se ----

get_se <- function() {
  SummarizedExperiment(
    list(counts = matrix(
      data = 1:4,
      nrow = 2,
      ncol = 2,
      dimnames = list(c("GeneID:a", "GeneID:b"), c("X", "Y"))
    )),
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
      LowExpressionFlag = c(TRUE, FALSE)
    ),
    colData = data.frame(
      SampleID = c(1, 2),
      LowDepthFlag = c(FALSE, FALSE),
      TechnicalFailureFlag = c(FALSE, TRUE)
    ),
    metadata = list(
      filename = "bla.txt",
      hash = "9352983502"
    )
  )
}

# get_rse ----

get_rse <- function() {
  SummarizedExperiment(
    list(counts = matrix(
      data = 1:4,
      nrow = 2,
      ncol = 2,
      dimnames = list(c("ENSGa", "ENSGb"), c("X", "Y"))
    )),
    rowRanges = GRanges(
      c("chr1", "chr2"),
      IRanges(c(124L, 134214L), width = 100),
      strand = c("+", "-"),
      feature_id = c(1L, 2L),
      HGNC = c(1, 1),
      GeneID = c(1, 1),
      Chromosome = c(1, 1),
      StartBP = c(1, 1),
      EndBP = c(1, 1),
      WidthBP = c(1, 1),
      HGNCGeneName = c(1, 1),
      CanonicalTranscript = c(1, 1),
      ProteinTranscript = c(1, 1),
      LowExpressionFlag = c(TRUE, FALSE)
    ),
    colData = data.frame(
      SampleID = c(1, 2),
      LowDepthFlag = c(FALSE, FALSE),
      TechnicalFailureFlag = c(FALSE, TRUE)
    ),
    metadata = list(
      filename = "bla.txt",
      hash = "9352983502"
    )
  )
}
