h_km_mae_to_adtte <- function(adtte,
                              mae,
                              genes,
                              experiment_name = "hd1",
                              assay_name = "counts",
                              usubjid_var = "USUBJID") {
  assert_class(mae, "MultiAssayExperiment")
  assert_string(experiment_name)
  assert_string(usubjid_var)
  assert_names(names(adtte), must.include = usubjid_var)

  # Check subject ID across experiment, sample map, and MAE colData.
  mae_samplemap <- MultiAssayExperiment::sampleMap(mae)
  samplemap_experiment <- mae_samplemap[mae_samplemap$assay == experiment_name, ]
  sm_usubjid <- as.character(samplemap_experiment$primary)

  hd <- suppressWarnings(MultiAssayExperiment::getWithColData(mae, experiment_name))
  assert_class(hd, "AnyHermesData")
  hd_usubjid <- as.character(SummarizedExperiment::colData(hd)[[usubjid_var]])

  assert_subset(
    x = hd_usubjid,
    choices = sm_usubjid
  )

  mae_coldata <- MultiAssayExperiment::colData(mae)
  if (usubjid_var %in% colnames(mae_coldata)) {
    mae_usubjid <- as.character(mae_coldata[[usubjid_var]])
    assert_subset(
      x = sm_usubjid,
      choices = mae_usubjid
    )
  }

  gene_data <- hermes::col_data_with_genes(
    object = hd,
    assay_name = assay_name,
    genes = genes
  )
  merged_adtte <- hermes::inner_join_cdisc(
    gene_data = gene_data,
    cdisc_data = adtte,
    patient_key = usubjid_var
  )
  structure(
    merged_adtte,
    gene_cols = attr(gene_data, "gene_cols")
  )
}

test_that("h_km_mae_to_adtte fails as expected if USUBJID in MAE colData is different from sample map", {
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list(
      a = hermes::HermesDataFromMatrix(
        matrix(1:4, 2, 2, dimnames = list(c("ENSG1", "ENSG2"), c("A", "B"))),
        colData = data.frame(USUBJID = c("A", "B")) # USUBJID on experiment.
      )
    ),
    colData = data.frame(
      USUBJID = c("C", "D"), # USUBJID on MAE.
      row.names = c("A", "B")
    )
  )
  adtte <- data.frame(USUBJID = c("A", "B"))

  expect_error(
    h_km_mae_to_adtte(
      adtte,
      mae,
      genes = hermes::gene_spec("ENSG1"),
      experiment_name = "a"
    ),
    "Must be a subset of {'C','D'}, but has additional elements {'A','B'}",
    fixed = TRUE
  )
})
