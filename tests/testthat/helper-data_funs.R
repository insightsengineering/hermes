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
      symbol = c(1, 1),
      chromosome = c(1, 1),
      size = c(1, 1),
      desc = c(1, 1),
      low_expression_flag = c(TRUE, FALSE)
    ),
    colData = data.frame(
      low_depth_flag = c(FALSE, FALSE),
      tech_failure_flag = c(FALSE, TRUE)
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
      symbol = c(1, 1),
      chromosome = c(1, 1),
      size = c(1, 1),
      desc = c(1, 1),
      low_expression_flag = c(TRUE, FALSE)
    ),
    colData = data.frame(
      low_depth_flag = c(FALSE, FALSE),
      tech_failure_flag = c(FALSE, TRUE)
    ),
    metadata = list(
      filename = "bla.txt",
      hash = "9352983502"
    )
  )
}
