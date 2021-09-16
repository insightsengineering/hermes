se_row_data_path <- file.path("data-raw", "summarized_experiment_rowData.csv")
se_col_data_path <- file.path("data-raw", "summarized_experiment_colData.csv")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_row_data <- read.csv(se_row_data_path, row.names = 1)
se_col_data <- read.csv(se_col_data_path, row.names = 1)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

counts <- as.matrix(se_counts)

expression_set <- ExpressionSet(
  assayData = as.environment(list(counts = counts, exprs = counts)),
  phenoData = AnnotatedDataFrame(se_col_data),
  featureData = AnnotatedDataFrame(se_row_data)
)

# ExpressionSet expects an assay called `exprs` during creation. However, the
# HermesData validator does expect one called `counts`. So we're getting rid of
# `exprs` here again.
remove("exprs", envir = expression_set@assayData)

usethis::use_data(expression_set, overwrite = TRUE)
