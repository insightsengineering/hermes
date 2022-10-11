test_that("draw_heatmap works as expected", {
  result <- draw_heatmap(
    hermes_data,
    assay_name = "counts",
    color_extremes = c(0.01, 0.99),
    col_data_annotation = "SEX",
  )

  vdiffr::expect_doppelganger("draw_heatmap with one col_data_annotation", result)
})
