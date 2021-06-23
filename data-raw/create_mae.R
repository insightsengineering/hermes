set.seed(100)

# Start with the example SE object.
se <- hermes::summarized_experiment

# Randomly add to the counts assay.
assay(se, "counts")[] <- assay(se, "counts")[]  + rnbinom(n = prod(dim(se)), size = 100, prob = 0.5)

# Subset the example SE object into 3 different SEs.
se1 <- se[1001:2000, 1:5]
se2 <- se[1001:3500, 6:14]
se3 <- se[1501:2800, 15:20]

# Create sample maps for each experiment.
se1map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John"),
  colname = c("06520011B0023R", "06520067C0018R", "06520063C0043R", "06520105C0017R", "06520092C0017R"),
  stringsAsFactors = FALSE
)

se2map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane", "Claire", "Mike", "Kate"),
  colname = c(
    "06520103C0017R", "06520001B0023R", "06520022C0017R", "06520062C0017R", "06520046C0018R",
    "06520101B0017R", "06520047C0017R", "06520024B0014R", "06520080B0023R"
  ),
  stringsAsFactors = FALSE
)

se3map <- data.frame(
  primary = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane"),
  colname = c("06520093C0017R", "06520070C0018R", "06520023C0018R", "06520099B0017R", "06520015C0016R", "06520019C0023R"),
  stringsAsFactors = FALSE
)

maplist <- list(se1 = se1map, se2 = se2map, se3 = se3map)
sampMap <- listToMap(maplist)

# Create an example phenotype data.
colDat <- data.frame(
  sex = c("M", "F", "F", "M", "M", "F", "F", "M", "F"),
  age = 35:43,
  row.names = c("Jack", "Jill", "Barbara", "Bob", "John", "Jane", "Claire", "Mike", "Kate")
)

# Create a named experiment list.
assaylist <- list(se1 = se1, se2 = se2, se3 = se3)
ExpList <- ExperimentList(assaylist)

# Create a MultiAssayExperiment object.
multi_assay_experiment <- MultiAssayExperiment(
  experiments = ExpList,
  colData = colDat,
  sampleMap = sampMap
)

# Save it in the package.
usethis::use_data(multi_assay_experiment, overwrite = TRUE)
