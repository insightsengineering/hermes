# If not on CI or Bioconductor,
# create object `.mart` with constant `Ensembl` version.
on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
on_bioc <- !(identical(Sys.getenv("BBS_HOME"), ""))
.mart <- if (on_ci || on_bioc) {
  NULL
} else {
  as(
    connect_biomart(version = "104"),
    "Mart"
  )
}

# To avoid SSL connection issues.
httr::set_config(httr::config(ssl_verifypeer = FALSE))

# connect_biomart ----

test_that("connect_biomart works as expected", {
  skip_on_ci()
  skip_on_bioc()

  result <- connect_biomart("ENSG")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "ENSG")

  result <- connect_biomart("GeneID")
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "GeneID")
})

test_that("connect_biomart can specify older version of Ensembl", {
  skip_on_ci()
  skip_on_bioc()

  result <- expect_silent(connect_biomart("ENSG", version = "103"))
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "ENSG")

  result <- expect_silent(connect_biomart("GeneID", version = "104"))
  expect_s4_class(result, "ConnectionBiomart")
  expect_identical(prefix(result), "GeneID")
})

# h_get_annotation_biomart ----

test_that("h_get_annotation_biomart works as expected", {
  skip_on_ci()
  skip_on_bioc()

  result <- h_get_annotation_biomart(
    gene_ids = c("11185", "10677"),
    id_var = "entrezgene_id",
    mart = .mart
  )
  expected <- data.frame(
    hgnc_symbol = c("INMT", "AVIL"),
    entrezgene_description = c("indolethylamine N-methyltransferase", "advillin"),
    chromosome_name = c(7L, 12L),
    size = c(3376L, 5889L),
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

# h_get_size_biomart ----

test_that("h_get_size_biomart works as expected", {
  skip_on_ci()
  skip_on_bioc()

  expect_identical(
    h_get_size_biomart(
      "11185",
      id_var = "entrezgene_id",
      mart = .mart
    ),
    c("11185" = 3376L)
  )
  expect_identical(
    h_get_size_biomart(
      "ENSG00000215417",
      id_var = "ensembl_gene_id",
      mart = .mart
    ),
    c(ENSG00000215417 = 3774L)
  )
  expect_identical(
    h_get_size_biomart(
      c("11185", "10677"),
      id_var = "entrezgene_id",
      mart = .mart
    ),
    c("11185" = 3376L, "10677" = 5889L)
  )
  expect_identical(
    h_get_size_biomart(
      c("ENSG00000135407", "ENSG00000215417"),
      id_var = "ensembl_gene_id",
      mart = .mart
    ),
    c(ENSG00000135407 = 5889L, ENSG00000215417 = 3774L)
  )
})

# h_ensembl_to_entrez_ids ----

test_that("h_ensembl_to_entrez_ids works as expected", {
  skip_on_ci()
  skip_on_bioc()

  result <- h_ensembl_to_entrez_ids(c("ENSG00000135407", "ENSG00000241644"), .mart)
  expected <- c("10677", "11185")
  expect_identical(result, expected)
})

# h_get_granges_by_id ----

test_that("h_get_granges_by_id works as expected", {
  coords <- data.frame(
    ensembl_gene_id = rep("ENSG00000135407", 11),
    ensembl_exon_id = c(
      "ENSE00002428433", "ENSE00002327057", "ENSE00002335328", "ENSE00002383516",
      "ENSE00003482354", "ENSE00003649447", "ENSE00003573463", "ENSE00003532761",
      "ENSE00002347218", "ENSE00002327284", "ENSE00002346343"
    ),
    chromosome_name = rep(12L, 11),
    exon_chrom_start = c(
      57799795L, 57797376L, 57815975L, 57815578L, 57814152L,
      57802160L, 57801144L, 57799795L, 57797882L, 57808395L, 57807331L
    ),
    exon_chrom_end = c(
      57800653L, 57797995L, 57816058L, 57815692L, 57814226L, 57802348L, 57801212L,
      57799920L, 57797995L, 57808692L, 57807727L
    )
  )
  result <- h_get_granges_by_id(coords, "ENSG00000135407")
  expect_s4_class(result, "GRanges")
  expect_identical(IRanges::start(result), coords$exon_chrom_start)
  expect_identical(IRanges::end(result), coords$exon_chrom_end)
})

# query-ConnectionBiomart ----

test_that("query to Biomart works as expected", {
  skip_on_ci()
  skip_on_bioc()

  object <- hermes_data[1:10, ]
  connection <- connect_biomart(prefix(object), version = "104")
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
