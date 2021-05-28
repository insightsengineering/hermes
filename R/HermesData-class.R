#' @include HermesData-validate.R
NULL

#' HermesData and RangedHermesData
#' 
#' The [HermesData] class is an extension of [SummarizedExperiment::SummarizedExperiment]
#' with additional validation criteria.
#' 
#' The additional criteria are:
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
#' Analogously, [RangedHermesData] is an extension of
#' [SummarizedExperiment::RangedSummarizedExperiment] and has the same
#' additional validation requirements. Methods can be defined for both classes at the
#' same time with the [AnyHermesData] signature.
#'   
#' A [Biobase::ExpressionSet] object can be imported by using the
#' [SummarizedExperiment::makeSummarizedExperimentFromExpressionSet] function to
#' first convert it to a [SummarizedExperiment::SummarizedExperiment] object before
#' converting it again into a [HermesData] object.
#' 
#' @examples
#' # Convert to SummarizedExperiment using the default naive range mapper.
#' se <- makeSummarizedExperimentFromExpressionSet(expression_set)
#' # Then convert to HermesData.
#'
#' @note Note that we use [S4Vectors::setValidity2()] to define the validity
#'   method, which allows us to turn off the validity checks in internal
#'   functions where intermediate objects may not be valid within the scope of
#'   the function.
#'   
#' @aliases HermesData RangedHermesData AnyHermesData
#' @exportClass HermesData RangedHermesData AnyHermesData
#' @importFrom S4Vectors setValidity2
#' 
.HermesData <- setClass(
  "HermesData",
  contains = "SummarizedExperiment"
)

#' @rdname HermesData-class
.RangedHermesData <- setClass(
  "RangedHermesData",
  contains = "RangedSummarizedExperiment"
)

#' @rdname HermesData-class
setClassUnion(
  name = "AnyHermesData",
  members = c("HermesData", "RangedHermesData")
)

S4Vectors::setValidity2("AnyHermesData", function(object) {
  msg <- NULL
  
  msg <- c(msg, validate_counts(object))
  msg <- c(msg, validate_row_data(object))
  msg <- c(msg, validate_col_data(object))
  
  if (is.null(msg)) TRUE else msg
})

#' @rdname HermesData-class
#' @param object (`SummarizedExperiment`)\cr input to create [HermesData] from.
#' @examples 
#' hermes_data <- HermesData(summarized_experiment)
#' hermes_data
#' 
HermesData <- function(object) {
  assert_that(
    is_class(object, "SummarizedExperiment"),
    not_empty(assays(object)),
    not_empty(rowData(object)),
    not_empty(colData(object))
  )
  
  missing_row <- setdiff(.row_data_additional_cols, names(rowData(object)))
  rowData(object)[, missing_row] <- NA
  
  missing_col <- setdiff(.col_data_additional_cols, names(colData(object)))
  colData(object)[, missing_col] <- NA
  
  .HermesData(object)
}
