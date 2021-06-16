#' @include HermesData-validate.R
NULL

# HermesData-class ----

#' `HermesData` and `RangedHermesData`
#'
#' The [`HermesData`] class is an extension of [`SummarizedExperiment::SummarizedExperiment`]
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
#' - The object must have row and column names.
#'
#' Analogously, [`RangedHermesData`] is an extension of
#' [`SummarizedExperiment::RangedSummarizedExperiment`] and has the same
#' additional validation requirements. Methods can be defined for both classes at the
#' same time with the [`AnyHermesData`] signature.
#'
#' A [`Biobase::ExpressionSet`] object can be imported by using the
#' [SummarizedExperiment::makeSummarizedExperimentFromExpressionSet()] function to
#' first convert it to a [`SummarizedExperiment::SummarizedExperiment`] object before
#' converting it again into a [`HermesData`] object.
#'
#' @examples
#' # Convert to `SummarizedExperiment` using the default naive range mapper.
#' se <- makeSummarizedExperimentFromExpressionSet(expression_set)
#' # Then convert to `HermesData`.
#' @note Note that we use [S4Vectors::setValidity2()] to define the validity
#'   method, which allows us to turn off the validity checks in internal
#'   functions where intermediate objects may not be valid within the scope of
#'   the function.
#'
#' @aliases HermesData RangedHermesData AnyHermesData
#' @exportClass HermesData RangedHermesData AnyHermesData
#' @importFrom S4Vectors setValidity2
#'
.HermesData <- setClass( # nolint
  "HermesData",
  contains = "SummarizedExperiment"
)

#' @rdname HermesData-class
.RangedHermesData <- setClass( # nolint
  "RangedHermesData",
  contains = "RangedSummarizedExperiment"
)

#' @rdname HermesData-class
setClassUnion(
  name = "AnyHermesData",
  members = c("HermesData", "RangedHermesData")
)

# HermesData-validity ----

S4Vectors::setValidity2("AnyHermesData", function(object) {
  msg <- NULL

  msg <- c(msg, validate_counts(object))
  msg <- c(msg, validate_row_data(object))
  msg <- c(msg, validate_col_data(object))
  msg <- c(msg, validate_names(object))

  if (is.null(msg)) TRUE else msg
})

# HermesData-constructors ----

#' @rdname HermesData-class
#' @param object (`SummarizedExperiment`)\cr input to create [`HermesData`] from.
#'   If this is a `RangedSummarizedExperiment`, then the result will be
#'   [`RangedHermesData`].
#' @export
#' @examples
#' # Create objects starting from a `SummarizedExperiment.`
#' hermes_data <- HermesData(summarized_experiment)
#' hermes_data
#' ranged_summarized_experiment <- as(summarized_experiment, "RangedSummarizedExperiment")
#' ranged_hermes_data <- HermesData(ranged_summarized_experiment)
#' ranged_hermes_data
HermesData <- function(object) { # nolint
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

  if (is(object, "RangedSummarizedExperiment")) {
    .RangedHermesData(object)
  } else {
    .HermesData(object)
  }
}

#' @rdname HermesData-class
#' @param counts (`matrix`)\cr counts to create the [`HermesData`] object from.
#' @param ... additional arguments, e.g. `rowData`, `colData`, etc. passed to
#'   [SummarizedExperiment::SummarizedExperiment()] internally. Note that if `rowRanges`
#'   is passed instead of `rowData`, then the result will be a [`RangedHermesData`] object.
#' @export
#' @examples
#' # Create objects from a matrix and additional arguments.
#' counts_matrix <- assay(summarized_experiment)
#' HermesDataFromMatrix(
#'   counts = counts_matrix,
#'   rowData = rowData(summarized_experiment),
#'   colData = colData(summarized_experiment)
#' )
HermesDataFromMatrix <- function(counts, ...) { # nolint
  assert_that(is.matrix(counts))

  # Note: `object` will either be of class `SummarizedExperiment` or
  # `RangedSummarizedExperiment`, depending on additional input arguments.
  object <- SummarizedExperiment(
    list(counts = counts),
    ...
  )
  HermesData(object)
}
