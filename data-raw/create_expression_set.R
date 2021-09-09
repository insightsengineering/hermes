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


# summarized_experiment <- new(
#   "SummarizedExperiment",
#   colData = DataFrame(se_colData),
#   assays = as(SimpleList(assay(as.matrix(se_counts))), "ShallowSimpleListAssays"),
#   NAMES = se_elementMetadata_listData$GeneID,
#   elementMetadata = DataFrame(se_elementMetadata_listData),
#   metadata = list()
# )


expression_set <- ExpressionSet(assayData = SimpleList(counts = assay(as.matrix(se_counts))),
                                phenoData = AnnotatedDataFrame(se_colData),
                                featureData = AnnotatedDataFrame(se_rowData))

usethis::use_data(expression_set, overwrite = TRUE)
