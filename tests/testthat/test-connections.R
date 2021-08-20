# connect_biomart ----

test_that("connect_biomart works as expected", {
  skip_if_too_deep(2)

  result <- connect_biomart("ENSG")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "ENSG")

  result <- connect_biomart("GeneID")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "GeneID")
})

# h_get_annotation_biomart ----

test_that("h_get_annotation_biomart works as expected", {
  skip_if_too_deep(2)

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
  test.nest::skip_if_too_deep(0)

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
  skip_if_too_deep(2)

  object <- HermesData(summarized_experiment)[1:10, ]
  connection <- connect_biomart(prefix(object))
  result <- query(genes(object), connection)
  expect_s4_class(result, "DataFrame")
  expect_identical(names(result), .row_data_annotation_cols)
  expect_identical(genes(object), rownames(result))
  result_subset <- result[3:4, ]
  expected_subset <- S4Vectors::DataFrame(
    HGNC = c(NA, "MIR3183"),
    HGNCGeneName = c(NA, "microRNA 3183"),
    Chromosome = c(NA, "17"),
    StartBP = c(NA, 1022476L),
    EndBP = c(NA, 1022559L),
    WidthBP = c(NA, 84L),
    CanonicalTranscript = c(NA_character_, NA_character_),
    ProteinTranscript = c(NA_character_, NA_character_),
    row.names = c("GeneID:101928428", "GeneID:100422835")
  )
  expect_identical(result_subset, expected_subset)
})
