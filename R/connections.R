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
#' \dontrun{
#' connection <- connect_biomart("ENSG")
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
#' @param gene_ids (`character`)\cr gene IDs, e.g. `10329`.
#' @param id_var (`string`)\cr corresponding gene ID variable name in BioMart,
#'   e.g. `entrezgene_id`.
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
#' \dontrun{
#' mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#' h_get_annotation_biomart(c("11185", "10677"), id_var = "entrezgene_id", mart = mart)
#' }
h_get_annotation_biomart <- function(gene_ids,
                                     id_var,
                                     mart) {
  assert_that(
    utils.nest::is_character_vector(gene_ids),
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
  entrez_ids <- as.character(df_gene$entrezgene_id)
  exon_sizes <- h_get_size_biomart(entrez_ids)
  df_gene <- dplyr::left_join(df_gene, exon_sizes, by = "entrezgene_id")
  df_gene <- df_gene[, -which(colnames(df_gene) == "ensembl_gene_id")]
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

#' Returns the Total Length of All Exons for a given ID
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function queries BioMart for the length of a gene by adding up all exon lengths after reducing overlaps.
#'
#' @param gene_ids (`character`)\cr gene ID(s) to query the size for, either Entrez or Ensembl ID.
#'
#' @return Named integer(s) indicating the gene length(s).
#' @export
#'
#' @examples
#' if(interactive()){
#'
#'   h_get_size_biomart("11185")
#'
#'   h_get_size_biomart("GeneID:11185")
#'
#'   h_get_size_biomart("ENSG00000215417")
#'
#'   h_get_size_biomart("ENSG00000215417.1")
#'
#'   h_get_size_biomart(c("GeneID:11185", "GeneID:10677"))
#'
#'   h_get_size_biomart(c("ENSG00000135407", "ENSG00000215417"))
#'
#' }
h_get_size_biomart <- function(gene_ids) {

  assert_character(gene_ids)
  attrs <- c(
    "ensembl_gene_id",
    "ensembl_exon_id",
    "chromosome_name",
    "exon_chrom_start",
    "exon_chrom_end"
  )

  mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  is_ensemble <- sum(grepl("ENSG", gene_ids)) > 0

  if (is_ensemble) {
    # Remove version number.
    gene_ids <- unlist(lapply(gene_ids, function(id) { gsub("\\.\\d+", "", id) }))
    biomart_filter <- "ensembl_gene_id"
  } else {
    # if it's not an ensembl id, it's an entrez id
    is_prefixed_entrez <- sum(grepl("GeneID", gene_ids)) > 0
    if (is_prefixed_entrez) { gene_ids <- h_strip_prefix(gene_ids, prefix = "GeneID") }
    biomart_filter <- "entrezgene_id"
  }
  coords <- biomaRt::getBM(
    filters = biomart_filter,
    attributes = attrs,
    values = gene_ids,
    mart = mart
  )
  ids <- unique(coords[, "ensembl_gene_id"])
  exons <- GenomicRanges::GRangesList(sapply(ids, h_get_granges_by_id, df = coords), compress = FALSE)
  unique_exons <- GenomicRanges::reduce(exons)
  unique_exon_sizes <- GenomicRanges::width(unique_exons)
  total_exon_size <- sum(unique_exon_sizes)
  result_df <- data.frame(
    ensembl_gene_id = names(total_exon_size),
    size = unlist(total_exon_size)
  )
  entrezgene_id_helper <- biomaRt::getBM(
    filters = "ensembl_gene_id",
    attributes = c("ensembl_gene_id", "entrezgene_id"),
    values = result_df$ensembl_gene_id ,
    mart = mart
  )
  dplyr::left_join(result_df, entrezgene_id_helper, by = "ensembl_gene_id")
}

#' Conversion of BioMart Coordinates into `GRanges`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function automatically extracts the chromosome number, the start position and the end position of transcripts
#' in given data.frame as returned by `biomaRt::getBM()` and converts them to `GRanges` objects.
#'
#' @param df (`data.frame`)\cr data.frame as returned by `biomaRt::getBM()`.
#' @param id (`character`)\cr gene ID(s) to convert the coordinates for.
#'
#' @return `GRange` objects for the respective gene ID(s).
#' @export
#'
#' @examples
#'
#' mart <- biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#' attrs <- c("ensembl_gene_id",
#'            "ensembl_exon_id",
#'            "chromosome_name",
#'            "exon_chrom_start",
#'            "exon_chrom_end")
#'
#' coords <- biomaRt::getBM(filters = "entrezgene_id",
#'                          attributes = attrs,
#'                          values = c("11185", "10677"),
#'                          mart = mart)
#'
#' h_get_granges_by_id(coords, "ENSG00000135407")
h_get_granges_by_id <- function(df, id) {
  exons <- df[df[, "ensembl_gene_id"] == id, c("chromosome_name", "exon_chrom_start", "exon_chrom_end")]
  GenomicRanges::GRanges(
    exons$chromosome_name,
    IRanges::IRanges(exons$exon_chrom_start, exons$exon_chrom_end)
  )
}

# query-ConnectionBiomart ----

#' @rdname query
#'
#' @export
#' @examples
#' \dontrun{
#' object <- hermes_data
#' connection <- connect_biomart(prefix(object))
#' result <- query(genes(object), connection)
#' head(result)
#' head(annotation(object))
#' }
setMethod(
  f = "query",
  signature = c(genes = "character", connection = "ConnectionBiomart"),
  definition = function(genes, connection) {
    pre <- prefix(connection)
    gene_ids <- switch(
      pre,
      GeneID = h_strip_prefix(genes, prefix = pre),
      ENSG = genes
    )
    id_var <- switch(
      pre,
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
