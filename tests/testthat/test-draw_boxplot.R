# draw_boxplot ----

test_that("draw_boxplot works when there are duplicate labels in gene spec", {
  genes <- gene_spec(c(A = "GeneID:11185", A = "GeneID:10677"))
  result <- draw_boxplot(
    hermes_data,
    assay_name = "counts",
    genes = genes,
    violin = TRUE
  )

  expect_snapshot(ggplot2::layer_data(result, 2))
})

# h_draw_boxplot_df ----

test_that("h_draw_boxplot_df works as expected", {
  result <- h_draw_boxplot_df(
    hermes_data,
    assay_name = "counts",
    genes = gene_spec("GeneID:11185"),
    x_var = "SEX",
    color_var = "RACE",
    facet_var = "AGE18"
  )
  expected <- data.frame(
    x = c(
      "F", "F", "M", "M", "M", "F", "F", "F",
      "F", "F", "M", "F", "F", "F", "F", "F", "M", "M", "M", "M"
    ),
    y = c(
      3, 66, 35, 10, 68, 123, 65, 10, 106, 23, 72, 4, 36,
      0, 50, 479, 235, 35, 75, 47
    ),
    fill = factor("GeneID:11185"),
    facet = c(
      ">= 18", "< 18", "< 18", "< 18", "< 18", ">= 18",
      ">= 18", "< 18", "< 18", "< 18", "< 18", "< 18", "< 18",
      ">= 18", "< 18", "< 18", "< 18", "< 18", "< 18", "< 18"
    ),
    color = c(
      "MULTIPLE", "UNKNOWN", "WHITE", "WHITE", "WHITE",
      "UNKNOWN", "UNKNOWN", "ASIAN", "BLACK OR AFRICAN AMERICAN",
      "WHITE", "UNKNOWN", "UNKNOWN", "WHITE", "ASIAN", "WHITE",
      "UNKNOWN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN",
      "UNKNOWN"
    )
  )
  expect_identical(result, expected)
})
