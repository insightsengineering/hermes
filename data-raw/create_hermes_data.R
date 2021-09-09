#se_elementMetadata_listData_path <- file.path("data-raw", "summarized_experiment_elementMetadata_listData.rda")
#se_colData_listData_path <- file.path("data-raw", "summarized_experiment_colData_listData.rda")
se_rowData_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_colData_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")


#se_elementMetadata_listData <- readRDS(se_elementMetadata_listData_path)
#se_colData_listData <- readRDS(se_colData_listData_path)
se_rowData <- read.csv(se_rowData_path, row.names = 1)
se_colData <- read.csv(se_colData_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

# Rename and drop columns as specified in https://github.com/insightsengineering/hermes/issues/31
se_rowData <- se_rowData %>%
  dplyr::select(-c(StartBP, EndBP, CanonicalTranscript, ProteinTranscript)) %>%
  dplyr::rename(symbol = HGNC) %>%
  dplyr::rename(desc = HGNCGeneName) %>%
  dplyr::rename(chromosome = Chromosome) %>%
  dplyr::rename(size = WidthBP) %>%
  dplyr::rename(low_expression_flag = LowExpressionFlag)

se_colData <- se_colData %>%
  dplyr::rename(low_depth_flag = LowDepthFlag) %>%
  dplyr::rename(tech_failure_flag = TechnicalFailureFlag)


# summarized_experiment <- new(
#   "SummarizedExperiment",
#   colData = DataFrame(se_colData),
#   assays = as(SimpleList(assay(as.matrix(se_counts))), "ShallowSimpleListAssays"),
#   NAMES = se_elementMetadata_listData$GeneID,
#   elementMetadata = DataFrame(se_elementMetadata_listData),
#   metadata = list()
# )


hermes_data <- SummarizedExperiment(assays = SimpleList(counts = assay(as.matrix(se_counts))),
                                    colData = DataFrame(se_colData),
                                    rowData = DataFrame(se_rowData))

hermes_data <- HermesData(hermes_data)

usethis::use_data(hermes_data, overwrite = TRUE)
