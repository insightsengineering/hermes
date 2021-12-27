#' Connection to BioMart
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `connect_biomart()` creates a connection object of class [`ConnectionBiomart`] which contains
#' the `biomaRt` object of class [`biomaRt::Mart`][biomaRt::Mart-class] and the prefix of the object
#' which is used downstream for the query.
#'
#' @details This connects to the Ensembl data base of BioMart for human genes.
#'
#' @param prefix (`string`)\cr gene ID prefix.
#'
#' @return [`ConnectionBiomart`] object.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   connection <- connect_biomart("ENSG")
#' }
connect_biomart <- function(prefix = c("ENSG", "GeneID")) {
  prefix <- match.arg(prefix)
  .ConnectionBiomart(
    biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl"),
    prefix = prefix
  )
}

#' @rdname connect_biomart
#' @aliases ConnectionBiomart
#' @importClassesFrom biomaRt Mart
#' @export
.ConnectionBiomart <- setClass(
  "ConnectionBiomart",
  contains = "Mart",
  slots = c(prefix = "character")
)

#' Get Annotations from BioMart
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function to query annotations from `biomaRt`, for cleaned up gene IDs of
#' a specific ID variable and given [`biomaRt::Mart`][biomaRt::Mart-class].
#'
#' @param gene_ids (`character`)\cr gene IDs, e.g. `10329`, i.e. already
#'   without the Entrez `GeneID` prefix, or `ENSG00000241644` for Ensembl gene ID.
#' @param id_var (`string`)\cr corresponding gene ID variable name in BioMart,
#'   i.e. `entrezgene_id` or `ensembl_gene_id`.
#' @param mart (`Mart`)\cr given [`biomaRt::Mart`][biomaRt::Mart-class] object.
#'
#' @return A data frame with columns:
#'   - `id_var` (depending on what was used)
#'   - `hgnc_symbol`
#'   - `entrezgene_description`
#'   - `chromosome_name`
#'   - `size`
#'   - `refseq_mrna`
#'   - `refseq_peptide`
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#'   h_get_annotation_biomart(c("11185", "10677"), id_var = "entrezgene_id", mart = mart)
#' }
h_get_annotation_biomart <- function(gene_ids,
                                     id_var,
                                     mart) {
  assert_character(gene_ids, any.missing = FALSE)
  assert_that(
    is.string(id_var),
    is(mart, "Mart")
  )
  df_gene <- biomaRt::getBM(
    attributes = c(
      id_var,
      "hgnc_symbol",
      "entrezgene_description",
      "chromosome_name"
    ),
    filters = id_var,
    values = gene_ids,
    mart = mart
  )
  gene_ids_values <- as.character(df_gene[[id_var]])
  df_gene$size <- h_get_size_biomart(gene_ids_values, id_var = id_var, mart = mart)
  df_protein <- biomaRt::getBM(
    attributes = c(
      id_var,
      "refseq_mrna",
      "refseq_peptide"
    ),
    filters = c(id_var, "transcript_is_canonical"),
    values = list(gene_ids, TRUE),
    mart = mart
  )
  df <- merge(df_gene, df_protein, by = id_var, all = TRUE)
  df <- df[match(gene_ids, df[[id_var]]), ]
  rownames(df) <- gene_ids
  df[, -which(colnames(df) == id_var)]
}

# query ----

#' Query Gene Annotations from a Connection
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The generic function `query()` is the interface for querying gene annotations from
#' a data base connection.
#'
#' @details
#' - A method is provided for the [`ConnectionBiomart`] class. However, the framework
#'   is extensible: It is simple to add new connections and corresponding query methods
#'   for other data bases, e.g. company internal data bases. Please make sure to
#'   follow the required format of the returned value.
#' - The BioMart queries might not return information for all the genes. This can be
#'   due to different versions being used in the gene IDs and the queried Ensembl data base.
#'
#' @param genes (`character`)\cr gene IDs.
#' @param connection (connection class)\cr data base connection object.
#'
#' @return A [`S4Vectors::DataFrame`] with the gene annotations. It is required that:
#'   - The `rownames` are identical to the input `genes`.
#'   - The `colnames` are equal to the annotation columns [`.row_data_annotation_cols`].
#'   - Therefore, missing information needs to be properly included in the `DataFrame`
#'     with `NA` entries.
#'
#' @export
setGeneric(
  "query",
  def = function(genes, connection) {
    value <- standardGeneric("query")
    assert_that(
      is(value, "DataFrame"),
      identical(genes, rownames(value)),
      all(.row_data_annotation_cols %in% colnames(value))
    )
    value
  }
)

#' Stripping Prefix from Gene IDs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function removes the prefix and possible delimiter from
#' a vector of gene IDs, such that only the digits are returned.
#'
#' @param gene_ids (`character`)\cr original gene IDs including prefix and optional
#'   delimiter before the digits.
#' @param prefix (`string`)\cr common prefix to be stripped away from `gene_ids`.
#'
#' @return Character vector that contains only the digits for each gene ID.
#' @export
#'
#' @note This is currently used to strip away the `GeneID` prefix from Entrez gene IDs
#'   so that they can be queried from BioMart
#'
#' @examples
#' h_strip_prefix(c("GeneID:11185", "GeneID:10677"), prefix = "GeneID")
h_strip_prefix <- function(gene_ids, prefix) {
  gsub(
    pattern = paste0("^", prefix, "[[:punct:]]?([[:digit:]]+)$"),
    replacement = "\\1",
    x = gene_ids
  )
}

#' Total Length of All Exons for Genes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function queries BioMart for lengths of genes by adding up all
#' exon lengths after reducing overlaps.
#'
#' @inheritParams h_get_annotation_biomart
#'
#' @return Named integer vector indicating the gene lengths.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#'   h_get_size_biomart("11185", "entrezgene_id", mart)
#'   h_get_size_biomart("ENSG00000215417", "ensembl_gene_id", mart)
#'   h_get_size_biomart(c("11185", "10677"), "entrezgene_id", mart)
#'   h_get_size_biomart(c("ENSG00000135407", "ENSG00000215417"), "ensembl_gene_id", mart)
#' }
h_get_size_biomart <- function(gene_ids,
                               id_var,
                               mart) {
  assert_character(gene_ids)
  assert_string(id_var)
  assert_subset(id_var, choices = c("ensembl_gene_id", "entrezgene_id"))
  assert_class(mart, "Mart")

  # We can only query this starting with Ensembl Gene IDs.
  attrs <- c(
    "ensembl_gene_id",
    "ensembl_exon_id",
    "chromosome_name",
    "exon_chrom_start",
    "exon_chrom_end"
  )
  coords <- biomaRt::getBM(
    filters = id_var,
    attributes = attrs,
    values = gene_ids,
    mart = mart
  )
  ids <- unique(coords[, "ensembl_gene_id"])
  granges_list <- lapply(
    X = ids,
    FUN = h_get_granges_by_id,
    coords = coords
  )
  names(granges_list) <- ids
  exons <- GenomicRanges::GRangesList(granges_list, compress = FALSE)
  unique_exons <- GenomicRanges::reduce(exons)
  unique_exon_sizes <- GenomicRanges::width(unique_exons)
  total_exon_size <- sum(unique_exon_sizes)

  if (id_var == "entrezgene_id") {
    # Translate names back to Entrez gene IDs.
    names(total_exon_size) <- h_ensembl_to_entrez_ids(
      names(total_exon_size),
      mart = mart
    )
  }

  # Make sure to return the sizes in the correct order.
  total_exon_size[gene_ids]
}

#' Translation of Ensembl to Entrez Gene IDs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function queries BioMart to translate Ensembl to Entrez Gene IDs.
#'
#' @param gene_ids (`character`)\cr Ensembl gene IDs.
#' @inheritParams h_get_annotation_biomart
#'
#' @return Character vector of Entrez gene IDs.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#'   h_ensembl_to_entrez_ids(c("ENSG00000135407", "ENSG00000241644"), mart)
#' }
h_ensembl_to_entrez_ids <- function(gene_ids,
                                    mart) {
  assert_character(gene_ids, pattern = "^ENSG")
  assert_class(mart, "Mart")

  translate_df <- biomaRt::getBM(
    filters = "ensembl_gene_id",
    attributes = c("ensembl_gene_id", "entrezgene_id"),
    values = gene_ids,
    mart = mart
  )
  result <- translate_df[match(gene_ids, translate_df$ensembl_gene_id), "entrezgene_id"]
  as.character(result)
}

#' Conversion of BioMart Coordinates into `GRanges`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function extracts the chromosome number, the start position and the end position of transcripts
#' in given `data.frame` with coordinates as returned by `biomaRt::getBM()` and converts
#' them to a `GRanges` object.
#'
#' @param coords (`data.frame`)\cr as returned by `biomaRt::getBM()`, containing the columns
#'   `ensembl_gene_id`, `chromosome_name`, `exon_chrom_start`, `exon_chrom_end`.
#' @param id (`string`)\cr single Ensembl gene ID to convert the coordinates for.
#'
#' @return `GRange` objects for the respective single gene ID.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#'   attrs <- c(
#'     "ensembl_gene_id",
#'     "ensembl_exon_id",
#'     "chromosome_name",
#'     "exon_chrom_start",
#'     "exon_chrom_end"
#'   )
#'   coords <- biomaRt::getBM(
#'     filters = "entrezgene_id",
#'     attributes = attrs,
#'     values = c("11185", "10677"),
#'     mart = mart
#'   )
#'   h_get_granges_by_id(coords, "ENSG00000135407")
#' }
h_get_granges_by_id <- function(coords, id) {
  assert_data_frame(coords)
  assert_names(
    names(coords),
    must.include = c("ensembl_gene_id", "chromosome_name", "exon_chrom_start", "exon_chrom_end")
  )
  assert_string(id)
  has_id <- coords[, "ensembl_gene_id"] == id
  assert_true(any(has_id))
  id_exons <- coords[has_id, c("chromosome_name", "exon_chrom_start", "exon_chrom_end")]
  GenomicRanges::GRanges(
    id_exons$chromosome_name,
    IRanges::IRanges(id_exons$exon_chrom_start, id_exons$exon_chrom_end)
  )
}

# query-ConnectionBiomart ----

#' @rdname query
#'
#' @export
#' @examples
#' if (interactive()) {
#'   object <- hermes_data
#'   connection <- connect_biomart(prefix(object))
#'   result <- query(genes(object), connection)
#'   head(result)
#'   head(annotation(object))
#' }
setMethod(
  f = "query",
  signature = c(genes = "character", connection = "ConnectionBiomart"),
  definition = function(genes, connection) {
    pre <- prefix(connection)
    gene_ids <- switch(pre,
      GeneID = h_strip_prefix(genes, prefix = pre),
      ENSG = genes
    )
    id_var <- switch(pre,
      GeneID = "entrezgene_id",
      ENSG = "ensembl_gene_id"
    )
    mart <- as(connection, "Mart")
    df <- h_get_annotation_biomart(gene_ids, id_var = id_var, mart = mart)
    with(
      df,
      S4Vectors::DataFrame(
        # Required annotations.
        symbol = hgnc_symbol,
        desc = entrezgene_description,
        chromosome = as.character(chromosome_name),
        size = size,
        # Additional annotations.
        canonical_transcript = refseq_mrna,
        protein_transcript = refseq_peptide,
        # Ensure correct row names.
        row.names = genes
      )
    )
  }
)
