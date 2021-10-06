# col_data_with_genes ----

test_that("col_data_with_genes works as expected", {
  result <- col_data_with_genes(hermes_data, "counts", gene_spec("GeneID:1820"))
  expect_identical(class(result), class(colData(hermes_data)))
  expect_names(names(result), must.include = c(
    names(colData(hermes_data)),
    "GeneID.1820"
  ))
  expect_identical(dim(result), dim(colData(hermes_data)) + c(0L, 1L))
})
