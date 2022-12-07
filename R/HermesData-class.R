#' @include HermesData-validate.R
NULL

# HermesData-class ----

#' `HermesData` and `RangedHermesData`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The [`HermesData`] class is an extension of [`SummarizedExperiment::SummarizedExperiment`]
#' with additional validation criteria.
#'
#' @details The additional criteria are:
#' - The first assay must be `counts` containing non-missing, integer, non-negative values.
#'   Note that [rename()] can be used to edit the assay name to `counts` if needed.
#' - The following columns must be in `rowData`:
#'   - `symbol` (also often called `HGNC` or similar, example: `"INMT"`)
#'   - `desc` (the gene name, example: `"indolethylamine N-methyltransferase"`)
#'   - `chromosome` (the chromosome as string, example: `"7"`)
#'   - `size` (the size of the gene in base pairs, e.g `5468`)
#'   - `low_expression_flag` (can be populated with [add_quality_flags()])
#' - The following columns must be in `colData`:
#'   - `low_depth_flag` (can be populated with [add_quality_flags()])
#'   - `tech_failure_flag` (can be populated with [add_quality_flags()])
#' - The object must have unique row and column names. The row names are the gene names
#'   and the column names are the sample names.
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
#' @note
#'   - Note that we use [S4Vectors::setValidity2()] to define the validity
#'   method, which allows us to turn off the validity checks in internal
#'   functions where intermediate objects may not be valid within the scope of
#'   the function.
#'   - It can be helpful to convert character and logical variables to factors in `colData()`
#'   (before or after the `HermesData` creation). We provide the utility function
#'   [df_cols_to_factor()] to simplify this task, but leave it to the user to allow
#'   for full control of the details.
#'
#' @slot prefix common prefix of the gene IDs (row names).
#'
#' @aliases HermesData RangedHermesData AnyHermesData
#' @exportClass HermesData RangedHermesData AnyHermesData
#'
#' @seealso [rename()] for renaming columns of the input data.
#'
#' @examples
#' # Convert an `ExpressionSet` to a `RangedSummarizedExperiment`.
#' ranged_summarized_experiment <- makeSummarizedExperimentFromExpressionSet(expression_set)
#'
#' # Then convert to `RangedHermesData`.
#' HermesData(ranged_summarized_experiment)
.HermesData <- setClass( # nolint
  "HermesData",
  contains = "SummarizedExperiment",
  slots = c(prefix = "character")
)

#' @rdname HermesData-class
.RangedHermesData <- setClass( # nolint
  "RangedHermesData",
  contains = "RangedSummarizedExperiment",
  slots = c(prefix = "character")
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
  msg <- c(msg, validate_prefix(object))

  if (is.null(msg)) TRUE else msg
})

# HermesData-constructors ----

#' @rdname HermesData-class
#' @param object (`SummarizedExperiment`)\cr input to create the [`HermesData`] object from.
#'   If this is a `RangedSummarizedExperiment`, then the result will be
#'   [`RangedHermesData`].
#' @return An object of class [`AnyHermesData`] ([`HermesData`] or [`RangedHermesData`]).
#'
#' @export
#' @examples
#'
#' # Create objects starting from a `SummarizedExperiment`.
#' hermes_data <- HermesData(summarized_experiment)
#' hermes_data
HermesData <- function(object) { # nolint
  assert_that(
    is_class(object, "SummarizedExperiment"),
    not_empty(assays(object))
  )

  assays(object) <- lapply(assays(object), function(x) {
    if (is(x, "DelayedMatrix")) {
      x <- as.matrix(x)
      mode(x) <- "integer"
    }
    x
  })

  missing_row <- setdiff(.row_data_cols, names(rowData(object)))
  rowData(object)[, missing_row] <- NA

  missing_col <- setdiff(.col_data_cols, names(colData(object)))
  colData(object)[, missing_col] <- NA

  gene_ids <- rownames(object)
  prefix <- if (all(grepl("^ENSG", gene_ids))) {
    "ENSG"
  } else if (all(grepl("^GeneID", gene_ids))) {
    "GeneID"
  } else {
    stop(
      "hermes requires either common prefix 'ENSG' (EnsemblID) ",
      "or 'GeneID' (EntrezID) for the row names (gene IDs)"
    )
  }

  if (is(object, "RangedSummarizedExperiment")) {
    .RangedHermesData(object, prefix = prefix)
  } else {
    .HermesData(object, prefix = prefix)
  }
}

#' @rdname HermesData-class
#' @param counts (`matrix`)\cr counts to create the [`HermesData`] object from.
#' @param ... additional arguments, e.g. `rowData`, `colData`, etc. passed to
#'   [SummarizedExperiment::SummarizedExperiment()] internally. Note that if `rowRanges`
#'   is passed instead of `rowData`, then the result will be a [`RangedHermesData`] object.
#' @export
#' @examples
#'
#' # Create objects from a matrix. Note that additional arguments are not required but possible.
#' counts_matrix <- assay(summarized_experiment)
#' counts_hermes_data <- HermesDataFromMatrix(counts_matrix)
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
