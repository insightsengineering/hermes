se_elementMetadata_listData_path <- file.path("data-raw", "summarized_experiment_elementMetadata_listData.rda")
se_colData_listData_path <- file.path("data-raw", "summarized_experiment_colData_listData.rda")
se_counts_path <- file.path("data-raw", "summarized_experiment_counts.csv")

se_elementMetadata_listData <- readRDS(se_elementMetadata_listData_path)
se_colData_listData <- readRDS(se_colData_listData_path)
se_counts <- read.csv(se_counts_path, row.names = 1, check.names = FALSE)

summarized_experiment <- new(
  "SummarizedExperiment",
  colData = new(
    "DataFrame",
    rownames = c(
      "06520011B0023R",
      "06520067C0018R",
      "06520063C0043R",
      "06520105C0017R",
      "06520092C0017R",
      "06520103C0017R",
      "06520001B0023R",
      "06520022C0017R",
      "06520062C0017R",
      "06520046C0018R",
      "06520101B0017R",
      "06520047C0017R",
      "06520024B0014R",
      "06520080B0023R",
      "06520093C0017R",
      "06520070C0018R",
      "06520023C0018R",
      "06520099B0017R",
      "06520015C0016R",
      "06520019C0023R"
    ),
    nrows = 20L,
    listData = se_colData_listData,
    elementType = "ANY",
    elementMetadata = NULL,
    metadata = list()
  ),
  assays = as(SimpleList(assay(as.matrix(se_counts))), "ShallowSimpleListAssays"),
  NAMES = se_elementMetadata_listData$GeneID,
  elementMetadata = new(
    "DataFrame",
    rownames = se_elementMetadata_listData$GeneID,
    nrows = 5085L,
    listData = se_elementMetadata_listData,
    elementType = "ANY",
    elementMetadata = NULL,
    metadata = list()
  ),
  metadata = list()
)

usethis::use_data(summarized_experiment, overwrite = TRUE)
