test_that("draw_boxplot works when there are duplicate labels in gene spec", {
  skip_on_ci()

  genes <- gene_spec(c(A = "GeneID:11185", A = "GeneID:10677"))
  result <- draw_boxplot(
    hermes_data,
    assay_name = "counts",
    genes = genes,
    violin = TRUE
  )

  vdiffr::expect_doppelganger("draw_boxplot with duplicate labels", result)
})
