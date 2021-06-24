#' Connection to IGIS
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Based on the prefix of the gene IDs, either creates a connection object of class
#' [`igis::Igis`] for version 4.0 (`ENSG`) or 3.0 (`GeneID`) (for human species genes).
#'
#' @param prefix (`string`)\cr gene ID prefix.
#'
#' @return [`igis::Igis`] object.
#'
#' @importFrom igis Igis
#' @export
#'
#' @examples
#' \dontrun{
#' connection <- connect_igis("ENSG")
#' }
connect_igis <- function(prefix = c("ENSG", "GeneID")) {
  prefix <- match.arg(prefix)
  version <- switch(
    prefix,
    ENSG = "4.0",
    GeneID = "3.0"
  )
  igis::Igis(species = "human", version = version)
}

# query ----

#' Query Gene Annotations from a Connection
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This generic function is the interface for querying gene annotations from
#' a data base connection. Currently only the IGIS version is implemented.
#'
#' @param genes (`character`)\cr gene IDs.
#' @param connection (`Igis` or other connection class)\cr data base connection object.
#'
#' @return A [`S4Vectors::DataFrame`] with the gene annotations.
#' @export
setGeneric("query", def = function(genes, connection) standardGeneric("query"))

# query-Igis ----

#' @rdname query
#' @importFrom igis transcriptsByGeneId
#' @export
#' @examples
#' \dontrun{
#' object <- HermesData(summarized_experiment)
#' connection <- connect_igis(prefix(object))
#' result <- query(genes(object), connection)
#' head(result)
#' }
setMethod(
  f = "query",
  signature = c(genes = "character", connection = "Igis"),
  definition = function(genes, connection) {
    transcripts <- igis::transcriptsByGeneId(
      geneIds = genes,
      canonical = TRUE,
      igis = connection
    )
    df <- cbind(
      setNames(
        mcols(transcripts),
        c(
          "HGNC",
          "GeneID",
          "CanonicalTranscript",
          "HGNCGeneName",
          "ProteinTranscript"
        )
      ),
      Chromosome = as.vector(seqnames(transcripts)),
      StartBP = start(transcripts),
      EndBP = end(transcripts),
      WidthBP = width(transcripts)
    )
    rownames(df) <- genes
    df[, .row_data_annotation_cols]
  }
)
