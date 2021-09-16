# connect_biomart ----

test_that("connect_biomart works as expected", {
  test.nest::skip_if_too_deep(5)

  result <- connect_biomart("ENSG")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "ENSG")

  result <- connect_biomart("GeneID")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "GeneID")
})

# h_get_annotation_biomart ----

test_that("h_get_annotation_biomart works as expected", {
  test.nest::skip_if_too_deep(3)

  mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  result <- h_get_annotation_biomart(
    gene_ids = c("11185", "10677"),
    id_var = "entrezgene_id",
    mart = mart
  )
  expected <- data.frame(
    hgnc_symbol = c("INMT", "AVIL"),
    entrezgene_description = c("indolethylamine N-methyltransferase", "advillin"),
    chromosome_name = c(7L, 12L),
    start_position = c(30697985L, 57797376L),
    end_position = c(30757602L, 57818704L),
    refseq_mrna = c("NM_006774", "NM_006576"),
    refseq_peptide = c("NP_006765", "NP_006567"),
    row.names = c("11185", "10677"),
    stringsAsFactors = FALSE
  )
  expect_identical(result, expected)
})

# h_strip_prefix ----

test_that("h_strip_prefix works as expected", {
  expect_identical(
    h_strip_prefix(c("GeneID:11185", "GeneID:10677"), prefix = "GeneID"),
    c("11185", "10677")
  )
  expect_identical(
    h_strip_prefix(c("GeneID11185", "GeneID10677"), prefix = "GeneID"),
    c("11185", "10677")
  )
  expect_identical(
    h_strip_prefix(c("g-1", "g-092301239000"), prefix = "g"),
    c("1", "092301239000")
  )
})

# query-ConnectionBiomart ----

test_that("query to Biomart works as expected", {
  test.nest::skip_if_too_deep(3)

  object <- hermes_data[1:10, ]
  connection <- connect_biomart(prefix(object))
  result <- query(genes(object), connection)
  expect_s4_class(result, "DataFrame")
  expect_subset(.row_data_annotation_cols, names(result))
  expect_identical(genes(object), rownames(result))
  result_subset <- result[3:4, ]
  expected_subset <- S4Vectors::DataFrame(
    symbol = c(NA, "MIR3183"),
    desc = c(NA, "microRNA 3183"),
    chromosome = c(NA, "17"),
    size = c(NA, 84L),
    canonical_transcript = as.character(c(NA, NA)),
    protein_transcript = as.character(c(NA, NA)),
    row.names = c("GeneID:101928428", "GeneID:100422835")
  )
  expect_identical(result_subset, expected_subset)
})
