test_that("draw_barplot works when there are duplicate labels in gene spec", {
  genes <- gene_spec(c(A = "GeneID:11185", A = "GeneID:10677"), fun = colMeans)
  result <- draw_barplot(
    hermes_data,
    assay_name = "counts",
    x_spec = genes,
    facet_var = "SEX",
    fill_var = "AGE18"
  )

  vdiffr::expect_doppelganger("draw_barplot with duplicate labels", result)
})
