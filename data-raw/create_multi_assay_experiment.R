library(hermes)
library(MultiAssayExperiment)
library(random.cdisc.data)

set.seed(100)

# Start with the example SE object.
se <- summarized_experiment

# Randomly add to the counts assay.
assay(se, "counts")[] <- assay(se, "counts")[] +
  rnbinom(n = prod(dim(se)), size = 100, prob = 0.5)

# Subset the example SE object into 3 different SEs.
se1 <- se[1001:2000, 1:5]
se2 <- se[1001:3500, 6:14]
se3 <- se[1501:2800, 15:20]

# Find suitable patient names.
adsl <- radsl(cached = TRUE)
# adsl$USUBJID[1:20]
pat_names <- c(
  "AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262", "AB12345-RUS-3-id-378",
  "AB12345-CHN-11-id-220", "AB12345-CHN-7-id-267", "AB12345-CHN-15-id-201",
  "AB12345-USA-1-id-45", "AB12345-USA-1-id-261", "AB12345-NGA-11-id-173",
  "AB12345-CHN-1-id-307", "AB12345-CHN-7-id-28", "AB12345-CHN-4-id-73",
  "AB12345-RUS-1-id-52", "AB12345-PAK-11-id-268", "AB12345-CHN-13-id-102",
  "AB12345-CHN-17-id-84", "AB12345-BRA-11-id-9", "AB12345-CHN-4-id-115",
  "AB12345-CHN-15-id-245", "AB12345-CHN-4-id-370"
)

# Create sample maps for each experiment.
se1map <- data.frame(
  primary = pat_names[1:5],
  colname = c("06520011B0023R", "06520067C0018R", "06520063C0043R", "06520105C0017R", "06520092C0017R"),
  stringsAsFactors = FALSE
)

se2map <- data.frame(
  primary = pat_names[6:14],
  colname = c(
    "06520103C0017R", "06520001B0023R", "06520022C0017R", "06520062C0017R", "06520046C0018R",
    "06520101B0017R", "06520047C0017R", "06520024B0014R", "06520080B0023R"
  ),
  stringsAsFactors = FALSE
)

se3map <- data.frame(
  primary = pat_names[c(15, 15, 16, 17, 18, 18)],
  colname = c("06520093C0017R", "06520070C0018R", "06520023C0018R", "06520099B0017R", "06520015C0016R", "06520019C0023R"),
  stringsAsFactors = FALSE
)

# Create an example phenotype data.
col_dat <- data.frame(
  sex = sample(c("M", "F"), size = 18, replace = TRUE),
  age = seq(from = 35, length = 18),
  row.names = pat_names[1:18]
)

# Remove SEX and AGE variables from experiment colData.
cd1 <- colData(se1)
colData(se1) <- cd1[, - match(c("SEX", "AGE"), names(cd1))]

cd2 <- colData(se2)
colData(se2) <- cd2[, - match(c("SEX", "AGE"), names(cd2))]

cd3 <- colData(se3)
colData(se3) <- cd3[, - match(c("SEX", "AGE"), names(cd3))]

# Make HermesData objects.
hd1 <- HermesData(se1)
hd2 <- HermesData(se2) %>%
  add_quality_flags() %>%
  filter() %>%
  normalize()
hd3 <- HermesData(se3)

# Create a named experiment list.
assaylist <- list(
  hd1 = hd1,
  hd2 = hd2,
  hd3 = hd3
)
ExpList <- ExperimentList(assaylist)

# Create a sample map.
maplist <- list(hd1 = se1map, hd2 = se2map, hd3 = se3map)
samp_map <- listToMap(maplist)

# Create a MultiAssayExperiment object.
multi_assay_experiment <- MultiAssayExperiment(
  experiments = ExpList,
  colData = col_dat,
  sampleMap = samp_map
)

# Save it in the package.
usethis::use_data(multi_assay_experiment, overwrite = TRUE)
