se_row_data_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_col_data_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_row_data <- read.csv(se_row_data_path, row.names = 1)
se_col_data <- read.csv(se_col_data_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

summarized_experiment <- SummarizedExperiment(
  assays = SimpleList(counts = assay(as.matrix(se_counts))),
  colData = DataFrame(se_col_data),
  rowData = DataFrame(se_row_data)
)

usethis::use_data(summarized_experiment, overwrite = TRUE)
