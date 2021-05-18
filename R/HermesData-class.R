#' @include HermesData-validate.R
NULL

#' HermesData
#' 
#' The [HermesData] class is an extension of [SummarizedExperiment::SummarizedExperiment]
#' with additional validation criteria:
#' 
#' - The first assay must be `counts` containing non-missing, integer, non-negative values.
#' - The following columns must be in `rowData`:
#'   - `HGNC`
#'   - `HGNCGeneName`
#'   - `GeneID`
#'   - `Chromosome`
#'   - `StartBP`
#'   - `EndBP`
#'   - `WidthBP`
#'   - `CanonicalTranscript`
#'   - `ProteinTranscript`
#'   - `LowExpressionFlag`
#' - The following columns must be in `colData`:
#'   - `SampleID`
#'   - `LowDepthFlag`
#'   - `TechnicalFailureFlag`
#'
#' @note Note that we use [S4Vectors::setValidity2()] to define the validity
#'   method, which allows us to turn off the validity checks in internal
#'   functions where intermediate objects may not be valid within the scope of
#'   the function.
#'   
#' @exportClass HermesData
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors setValidity2
#'
.HermesData <- setClass(
  "HermesData",
  contains = "SummarizedExperiment"
)

S4Vectors::setValidity2("HermesData", function(object) {
  msg <- NULL
  
  msg <- c(msg, validate_counts(object))
  msg <- c(msg, validate_row_data(object))
  msg <- c(msg, validate_col_data(object))
  
  if (is.null(msg)) TRUE else msg
})
