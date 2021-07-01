#' Connection to Biomart
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `connect_biomart()` creates a connection object of class [`ConnectionBiomart`] which contains
#' the `biomaRt` object of class [`biomaRt::Mart`][biomaRt::Mart-class] and the prefix of the object
#' which is used downstream for the query.
#'
#' @details This connects to the Ensembl data base of Biomart for human genes.
#'
#' @param prefix (`string`)\cr gene ID prefix.
#'
#' @return [`ConnectionBiomart`] object.
#'
#' @importFrom biomaRt useMart
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
.ConnectionBiomart <- setClass( # nolint
  "ConnectionBiomart",
  contains = "Mart",
  slots = c(prefix = "character")
)

#' Get Annotations from Biomart
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function to query annotations from `biomaRt`, for cleaned up gene IDs of
#' a specific ID variable and given [`biomaRt::Mart`][biomaRt::Mart-class].
#'
#' @param gene_ids (`character`)\cr gene IDs, e.g. `10329`.
#' @param id_var (`string`)\cr corresponding gene ID variable name in Biomart,
#'   e.g. `entrezgene_id`.
#' @param mart (`Mart`)\cr given [`biomaRt::Mart`][biomaRt::Mart-class] object.
#'
#' @return A data frame with columns:
#'   - `id_var` (depending on what was used)
#'   - `hgnc_symbol`
#'   - `entrezgene_description`
#'   - `chromosome_name`
#'   - `start_position`
#'   - `end_position`
#'   - `refseq_mrna`
#'   - `refseq_peptide`
#'
#' @importFrom utils.nest is_character_vector
#' @importFrom biomaRt getBM
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
      "chromosome_name",
      "start_position",
      "end_position"
    ),
    filters = id_var,
    values = gene_ids,
    mart = mart
  )
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
  df[, - which(colnames(df) == id_var)]
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
#' - The Biomart queries might not return information for all the genes. This can be
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
      setequal(.row_data_annotation_cols, colnames(value))
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
#'   so that they can be queried from Biomart.
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

# query-ConnectionBiomart ----

#' @rdname query
#' @importFrom S4Vectors DataFrame
#' @export
#' @examples
#' \dontrun{
#' object <- HermesData(summarized_experiment)
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
        HGNC = hgnc_symbol,
        HGNCGeneName = entrezgene_description,
        Chromosome = as.character(chromosome_name),
        StartBP = start_position,
        EndBP = end_position,
        WidthBP = end_position - start_position + 1L,
        CanonicalTranscript = refseq_mrna,
        ProteinTranscript = refseq_peptide,
        row.names = genes
      )[, .row_data_annotation_cols] # Ensure correct column order.
    )
  }
)
