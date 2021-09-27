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

  mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl") # nolint
  result <- h_get_annotation_biomart(
    gene_ids = c("11185", "10677"),
    id_var = "entrezgene_id",
    mart = mart
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
  test.nest::skip_if_too_deep(5)

  expect_identical(
    h_get_size_biomart("11185"),
    c(ENSG00000241644 = 3376L)
  )
  expect_identical(
    h_get_size_biomart("GeneID:11185"),
    c(ENSG00000241644 = 3376L)
  )
  expect_identical(
    h_get_size_biomart("ENSG00000215417"),
    c(ENSG00000215417 = 3774L)
  )
  expect_identical(
    h_get_size_biomart("ENSG00000215417.1"),
    c(ENSG00000215417 = 3774L)
  )
  expect_identical(
    h_get_size_biomart(c("GeneID:11185", "GeneID:10677")),
    c(ENSG00000135407 = 5889L, ENSG00000241644 = 3376L)
  )
  expect_identical(
    h_get_size_biomart(c("ENSG00000135407", "ENSG00000215417")),
    c(ENSG00000135407 = 5889L, ENSG00000215417 = 3774L)
  )
})

# h_get_granges_by_id ----

test_that("h_get_granges_by_id works as expected", {

  mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl") # nolint
  attrs <- c("ensembl_gene_id",
             "ensembl_exon_id",
             "chromosome_name",
             "exon_chrom_start",
             "exon_chrom_end")

  coords <- biomaRt::getBM( # nolint
    filters = "entrezgene_id",
    attributes = attrs,
    values = c("11185", "10677"),
    mart = mart
  )

  expected_output <- new(
    "GRanges",
    seqnames = new(
      "Rle",
      values = structure(1L, .Label = "12", class = "factor"),
      lengths = 57L,
      elementMetadata = NULL,
      metadata = list()
    ),
    ranges = new(
      "IRanges",
      start = c(
        57799795L,
        57797376L,
        57815975L,
        57815578L,
        57814152L,
        57813227L,
        57811019L,
        57810816L,
        57810349L,
        57809812L,
        57809597L,
        57808395L,
        57808194L,
        57807590L,
        57807331L,
        57806360L,
        57803524L,
        57803247L,
        57802160L,
        57801144L,
        57799795L,
        57797740L,
        57801144L,
        57797778L,
        57815975L,
        57814152L,
        57813227L,
        57811019L,
        57810816L,
        57810349L,
        57809812L,
        57809597L,
        57808395L,
        57808194L,
        57807590L,
        57807331L,
        57806360L,
        57803524L,
        57803247L,
        57802160L,
        57801144L,
        57799795L,
        57797882L,
        57808395L,
        57807331L,
        57802183L,
        57806360L,
        57803296L,
        57808194L,
        57805856L,
        57807590L,
        57806132L,
        57814152L,
        57806138L,
        57818629L,
        57815975L,
        57810901L
      ),
      width = c(
        859L,
        620L,
        84L,
        115L,
        75L,
        197L,
        109L,
        111L,
        203L,
        79L,
        99L,
        154L,
        101L,
        138L,
        159L,
        180L,
        146L,
        145L,
        189L,
        69L,
        126L,
        256L,
        274L,
        218L,
        497L,
        75L,
        197L,
        109L,
        111L,
        203L,
        79L,
        99L,
        154L,
        101L,
        138L,
        159L,
        180L,
        146L,
        145L,
        189L,
        69L,
        126L,
        114L,
        298L,
        397L,
        166L,
        130L,
        96L,
        986L,
        712L,
        52L,
        408L,
        75L,
        402L,
        76L,
        85L,
        26L
      ),
      NAMES = NULL,
      elementType = "ANY",
      elementMetadata = NULL,
      metadata = list()
    ),
    strand = new(
      "Rle",
      values = structure(3L, .Label = c("+", "-", "*"), class = "factor"),
      lengths = 57L,
      elementMetadata = NULL,
      metadata = list()
    ),
    seqinfo = new(
      "Seqinfo",
      seqnames = "12",
      seqlengths = NA_integer_,
      is_circular = NA,
      genome = NA_character_
    ),
    elementMetadata = new(
      "DFrame",
      rownames = NULL,
      nrows = 57L,
      listData = structure(list(), .Names = character(0)),
      elementType = "ANY",
      elementMetadata = NULL,
      metadata = list()
    ),
    elementType = "ANY",
    metadata = list()
  )

  expect_identical(
    h_get_granges_by_id(coords, "ENSG00000135407"),
    expected_output
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
