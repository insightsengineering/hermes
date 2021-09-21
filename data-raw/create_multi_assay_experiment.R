library(hermes)
library(MultiAssayExperiment)
library(random.cdisc.data)

set.seed(100)

# Start with the example HermesData object.
hd <- hermes_data

# Randomly add to the counts assay.
assay(hd, "counts")[] <- assay(hd, "counts")[] +
  rnbinom(n = prod(dim(hd)), size = 100, prob = 0.5)

# Subset the example SE object into 3 different SEs.
hd1 <- hd[1001:2000, 1:5]
hd2 <- hd[1001:3500, 6:14]
hd3 <- hd[1501:2800, 15:20]

# Find suitable patient names.
adsl <- radsl(cached = TRUE)

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
hd1map <- data.frame(
  primary = pat_names[1:5],
  colname = c("06520011B0023R", "06520067C0018R", "06520063C0043R", "06520105C0017R", "06520092C0017R"),
  stringsAsFactors = FALSE
)

hd2map <- data.frame(
  primary = pat_names[6:14],
  colname = c(
    "06520103C0017R", "06520001B0023R", "06520022C0017R", "06520062C0017R", "06520046C0018R",
    "06520101B0017R", "06520047C0017R", "06520024B0014R", "06520080B0023R"
  ),
  stringsAsFactors = FALSE
)

hd3map <- data.frame(
  primary = pat_names[c(15, 15, 16, 17, 18, 18)],
  colname = c(
    "06520093C0017R", "06520070C0018R", "06520023C0018R",
    "06520099B0017R", "06520015C0016R", "06520019C0023R"
  ),
  stringsAsFactors = FALSE
)

# Migrating subject level phenotype variables from experiment level to subject level
col_dat <- rbind(as.data.frame(colData(hd1)), as.data.frame(colData(hd2)), as.data.frame(colData(hd3)))[-c(16, 20), ]
drop_vars <- c("low_depth_flag", "SampleID", "tech_failure_flag")
col_dat <- col_dat[, !names(col_dat) %in% drop_vars]
rownames(col_dat) <- pat_names[1:18]

col_dat <- col_dat %>%
  dplyr::mutate(
    USUBJID = rownames(col_dat),
    SUBJID = rownames(col_dat)
  )

# Remove phenotype variables now at subject level from experiment colData.
cd1 <- colData(hd1)
colData(hd1) <- cd1[, - match(names(col_dat), names(cd1))]

cd2 <- colData(hd2)
colData(hd2) <- cd2[, - match(names(col_dat), names(cd2))]

cd3 <- colData(hd3)
colData(hd3) <- cd3[, - match(names(col_dat), names(cd3))]

# Add normalized assays to the second `HermesData` object.
hd2 <- hd2 %>%
  add_quality_flags() %>%
  filter() %>%
  normalize()

# Create a named experiment list.
assaylist <- list(
  hd1 = hd1,
  hd2 = hd2,
  hd3 = hd3
)
exp_list <- ExperimentList(assaylist)

# Create a sample map.
maplist <- list(hd1 = hd1map, hd2 = hd2map, hd3 = hd3map)
samp_map <- listToMap(maplist)

# Create a MultiAssayExperiment object.
multi_assay_experiment <- MultiAssayExperiment(
  experiments = exp_list,
  colData = col_dat,
  sampleMap = samp_map
)

# Save it in the package.
usethis::use_data(multi_assay_experiment, overwrite = TRUE)
