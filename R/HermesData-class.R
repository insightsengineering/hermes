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
#' @aliases HermesData
#' @exportClass HermesData
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


HermesData <- function(SummarizedExperiment.object) {
  
  assays <- assay(SummarizedExperiment.object)
  
  # If assay is not in the object
  if(length(assays) == 0L) stop("Input SummarizedExperiment object has no assay data, please check input.")
  
  # if assay is not in the numeric data
  if (!is.numeric(assays)) stop("Input SummarizedExperiment object should contain assay with only numerical data, please check inputs.")
  
  # if assay has missing data
  if (any(is.na(assays))) stop("Input SummarizedExperiment object should not contain assay with missing data, please check inputs.")
  
  # if assay has non-integer data
  if (!is.integer(assays)) stop("Input SummarizedExperiment object should not contain assay with non-integer data, please check inputs.")
 
  # if assay has negative data
  if (any(assays<0)) stop("Input SummarizedExperiment object should not contain assay with negative data, please check inputs.")
  
  
  # At this point, we don't look up GeneID row data. It will be updated later 
  rowData.char <- c("HGNC", "HGNCGeneName", "Chromosome",
                    "CanonicalTranscript", "ProteinTranscript")
  colData.char <- c("Filename", "SampleID")
  
  rowData.intbool <- c( "StartBP", "EndBP", "WidthBP", "LowExpressionFlag" )
  colData.intbool <- c( "LowDepthFlag", "TechnicalFailureFlag" )
  
  
  for ( r in rowData.char )
    if ( !( r %in% colnames( rowData( SummarizedExperiment.object ) ) ) )
      rowData( SummarizedExperiment.object )[[ r ]] <- rep( NA_character_, nrow( SummarizedExperiment.object ) ) # Be sure to handle length 0!
  
  for ( c in colData.char )
    if ( !( c %in% colnames( colData( SummarizedExperiment.object ) ) ) )
      colData( SummarizedExperiment.object )[[ c ]] <- rep( NA_character_, ncol( SummarizedExperiment.object ) )
  
  for ( r in rowData.intbool )
    if ( !( r %in% colnames( rowData( SummarizedExperiment.object ) ) ) )
      rowData( SummarizedExperiment.object )[[ r ]] <- rep( NA, nrow( SummarizedExperiment.object ) ) # Be sure to handle length 0!
  
  for ( c in colData.intbool )
    if ( !( c %in% colnames( colData( SummarizedExperiment.object ) ) ) )
      colData( SummarizedExperiment.object )[[ c ]] <- rep( NA, ncol( SummarizedExperiment.object ) )
  
  .HermesData( SummarizedExperiment.object )
}







