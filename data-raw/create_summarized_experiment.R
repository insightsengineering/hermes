se_rowData_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_colData_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_rowData <- read.csv(se_rowData_path, row.names = 1)
se_colData <- read.csv(se_colData_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

summarized_experiment <- SummarizedExperiment(assays = SimpleList(counts = assay(as.matrix(se_counts))),
                                              colData = DataFrame(se_colData),
                                              rowData = DataFrame(se_rowData))

usethis::use_data(summarized_experiment, overwrite = TRUE)
