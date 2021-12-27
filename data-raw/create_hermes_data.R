se_row_data_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_col_data_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_row_data <- read.csv(se_row_data_path, row.names = 1)
se_col_data <- read.csv(se_col_data_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

# Rename and drop columns as specified in https://github.com/insightsengineering/hermes/issues/31
se_row_data <- se_row_data %>%
  dplyr::select(-c(StartBP, EndBP, CanonicalTranscript, ProteinTranscript)) %>%
  dplyr::rename(symbol = HGNC) %>%
  dplyr::rename(desc = HGNCGeneName) %>%
  dplyr::rename(chromosome = Chromosome) %>%
  dplyr::rename(size = WidthBP) %>%
  dplyr::rename(low_expression_flag = LowExpressionFlag)

se_col_data <- se_col_data %>%
  dplyr::rename(low_depth_flag = LowDepthFlag) %>%
  dplyr::rename(tech_failure_flag = TechnicalFailureFlag)

hermes_data <- SummarizedExperiment(
  assays = SimpleList(counts = assay(as.matrix(se_counts))),
  colData = DataFrame(se_col_data),
  rowData = DataFrame(se_row_data)
)

hermes_data <- HermesData(hermes_data)

usethis::use_data(hermes_data, overwrite = TRUE)
