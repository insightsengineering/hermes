test_that("expression_set can be used", {
  dat <- expect_silent(expression_set)
  expect_is(dat, "ExpressionSet")
})

test_that("hermes_data can be used", {
  dat <- expect_silent(hermes_data)
  expect_is(dat, "HermesData")
})

test_that("summarized_experiment can be used", {
  dat <- expect_silent(summarized_experiment)
  expect_is(dat, "SummarizedExperiment")
})

test_that("hermes_data is a subset of summarized_experiment", {
  dat_se <- expect_silent(summarized_experiment)
  dat_hd <- expect_silent(hermes_data)

  # Used to ignore changes in colnames
  strip_df <- function(x) unname(as.data.frame(x))

  row_data_se <- rowData(dat_se) %>%
    as.data.frame() %>%
    dplyr::select(-c(StartBP, EndBP, CanonicalTranscript, ProteinTranscript))

  expect_equal(strip_df(row_data_se), strip_df(rowData(dat_hd)))
  expect_equal(strip_df(colData(dat_se)), strip_df(colData(dat_hd)))
  expect_equal(dat_se@assays@data$counts, assay(dat_hd))
})
