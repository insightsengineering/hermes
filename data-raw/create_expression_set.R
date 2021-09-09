se_rowData_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_colData_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_rowData <- read.csv(se_rowData_path, row.names = 1)
se_colData <- read.csv(se_colData_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

counts = as.matrix(se_counts)

expression_set <- ExpressionSet(assayData = as.environment(list(counts = counts, exprs = counts)),
                                phenoData = AnnotatedDataFrame(se_colData),
                                featureData = AnnotatedDataFrame(se_rowData))

# ExpressionSet expects an assay called `exprs` during creation. However, the
# HermesData validator does expect one called `counts`. So we're getting rid of
# `exprs` here again.
remove("exprs", envir = expression_set@assayData)

usethis::use_data(expression_set, overwrite = TRUE)
