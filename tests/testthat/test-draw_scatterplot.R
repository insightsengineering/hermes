test_that("draw_scatterplot works when there are duplicate labels in gene specs", {
  skip_on_ci()

  genes_x <- gene_spec(c(A = "GeneID:11185"))
  genes_y <- gene_spec(c(A = "GeneID:10677"))
  result <- draw_scatterplot(
    hermes_data,
    assay_name = "counts",
    facet_var = NULL,
    x_spec = genes_x,
    y_spec = genes_y,
    color = "RACE"
  )
  # Note that here it is ok that both x and y axis have the same label, as the user
  # controls via the arguments where is what, same for downstream teal module.
  expect_class(result, "ggplot")
})
