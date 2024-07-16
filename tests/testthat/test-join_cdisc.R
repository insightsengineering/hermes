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

test_that("col_data_with_genes works with duplicate gene names", {
  result <- col_data_with_genes(
    hermes_data,
    "counts",
    gene_spec(c(A = "GeneID:11185", A = "GeneID:10677"))
  )
  expect_names(names(result), must.include = c(
    names(colData(hermes_data)),
    c("A..GeneID.11185.", "A..GeneID.10677.")
  ))
  expect_identical(
    attr(result, "gene_cols"),
    c("A..GeneID.11185.", "A..GeneID.10677.")
  )
  expect_identical(dim(result), dim(colData(hermes_data)) + c(0L, 2L))
})

# inner_join_cdisc ----

test_that("inner_join_cdisc works as expected in the simplest case", {
  gene_data <- data.frame(
    USUBJID = 1:3,
    a = 5:3
  )
  cdisc_data <- data.frame(
    USUBJID = 1:3,
    b = 7:9
  )
  result <- expect_silent(inner_join_cdisc(gene_data, cdisc_data))
  expected <- data.frame(
    USUBJID = 1:3,
    a = 5:3,
    b = 7:9
  )
  expect_identical(result, expected)
})

test_that("inner_join_cdisc takes preferentially the gene_data columns in case of name overlap", {
  gene_data <- data.frame(
    USUBJID = 1:3,
    a = 5:3
  )
  cdisc_data <- data.frame(
    USUBJID = 1:3,
    a = 7:9
  )
  result <- expect_silent(inner_join_cdisc(gene_data, cdisc_data))
  expected <- gene_data
  expect_identical(result, expected)
})

test_that("inner_join_cdisc warns when patients are lost from gene_data", {
  gene_data <- data.frame(
    USUBJID = 1:3,
    a = 5:3
  )
  cdisc_data <- data.frame(
    USUBJID = 1:2,
    b = 7:8
  )
  expect_warning(
    result <- inner_join_cdisc(gene_data, cdisc_data),
    "Patients 3 from gene data set were lost"
  )
  expected <- data.frame(
    USUBJID = 1:2,
    a = 5:4,
    b = 7:8
  )
  expect_identical(result, expected)
})

test_that("inner_join_cdisc works correctly when custom patient key is specified", {
  gene_data <- data.frame(
    ID = 1:3,
    a = 5:3
  )
  cdisc_data <- data.frame(
    ID = 1:3,
    b = 7:9
  )
  result <- expect_silent(inner_join_cdisc(gene_data, cdisc_data, patient_key = "ID"))
  expected <- data.frame(
    ID = 1:3,
    a = 5:3,
    b = 7:9
  )
  expect_identical(result, expected)
})

test_that("inner_join_cdisc works correctly when additional keys are specified", {
  gene_data <- data.frame(
    ID = c(1, 2, 3, 1, 2, 3),
    TIME = c(1, 1, 1, 2, 2, 2),
    a = LETTERS[1:6]
  )
  cdisc_data <- data.frame(
    ID = c(1, 2, 3, 1, 2, 3),
    TIME = c(2, 2, 2, 1, 1, 0),
    b = letters[1:6]
  )
  result <- expect_silent(inner_join_cdisc(
    gene_data,
    cdisc_data,
    patient_key = "ID",
    additional_keys = "TIME"
  ))
  expected <- data.frame(
    ID = c(1, 1, 2, 2, 3),
    TIME = c(1, 2, 1, 2, 2),
    a = c("A", "D", "B", "E", "F"),
    b = c("d", "a", "e", "b", "c")
  )
  expect_identical(result, expected)
})
