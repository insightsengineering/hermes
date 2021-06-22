# connect_igis ----

test_that("connect_igis works as expected", {
  skip_if_too_deep(2)

  result <- connect_igis("ENSG")
  expect_s4_class(result, "Igis")

  result <- connect_igis("GeneID")
  expect_s4_class(result, "Igis")
})

# query-Igis ----

test_that("query to Igis works as expected", {
  skip_if_too_deep(2)

  object <- HermesData(summarized_experiment)
  connection <- connect_igis(prefix(object))
  result <- query(genes(object), connection)
  expect_s4_class(result, "DataFrame")
  expect_identical(names(result), .row_data_annotation_cols)
  expect_identical(genes(object), rownames(result))
})
