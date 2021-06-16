#' Internal Helper Functions for Validation of `AnyHermesData` Objects
#'
#' These functions are used internally only and therefore not exported. They work on
#' [`SummarizedExperiment::SummarizedExperiment`] objects, and [`AnyHermesData`] objects are
#' defined by successfully passing these validation checks.
#'
#' @name validate
#' @return A character vector with the validation failure messages, or `NULL` in
#'   case validation passes.
NULL

# Constants which are used in multiple places.
.row_data_non_empty_cols <- c("HGNC", "GeneID", "Chromosome", "StartBP", "EndBP", "WidthBP")
.row_data_additional_cols <- c("HGNCGeneName", "CanonicalTranscript", "ProteinTranscript", "LowExpressionFlag")
.col_data_non_empty_cols <- c("SampleID")
.col_data_additional_cols <- c("LowDepthFlag", "TechnicalFailureFlag")

#' @describeIn validate validates that the first assay is `counts` containing
#'   non-missing, integer, non-negative values.
#' @param object (`SummarizedExperiment`)\cr object to validate.
#'
validate_counts <- function(object) {
  nams <- assayNames(object)
  if (!("counts" %in% nams)) {
    return("no 'counts' assay found")
  }
  if (nams[1] != "counts") {
    return("'counts' must be the first assay")
  }

  msg <- NULL

  counts <- assay(object)
  if (!is.integer(counts)) {
    msg <- c(msg, "'counts' must be numeric in integer mode")
  }
  if (any(is.na(counts))) {
    msg <- c(msg, "missing values in 'counts'")
  }
  if (any(counts < 0)) {
    msg <- c(msg, "negative values in 'counts'")
  }

  msg
}

#' @describeIn validate validates that required column names are contained in
#'   actual column names.
#' @param required (`character`)\cr required column names.
#' @param actual (`actual`)\cr actual column names.
#'
validate_cols <- function(required, actual) {
  if (!all(required %in% actual)) {
    missing <- setdiff(required, actual)
    paste("required columns", paste(missing, collapse = ", "), "not present")
  } else {
    NULL
  }
}

#' @describeIn validate checks whether the whole vector is `NA`.
#' @param x (`vector`)\cr vector to check.
#'
all_na <- function(x) all(is.na(x))

#' @describeIn validate validates that the data frame is not containing only
#'   `NA`, per column.
#' @param df (`data.frame`)\cr data frame to validate.
#'
validate_non_empty <- function(df) {
  is_all_na <- vapply(df, all_na, TRUE)
  if (any(is_all_na)) {
    cols_all_na <- names(df)[is_all_na]
    paste("columns", paste(cols_all_na, collapse = ", "), "only contain NAs")
  } else {
    NULL
  }
}

#' @describeIn validate validates that the object contains `rowData` with
#'   required columns.
#'
validate_row_data <- function(object) {
  msg <- NULL

  non_empty_cols <- .row_data_non_empty_cols
  additional_cols <- .row_data_additional_cols
  required_cols <- c(non_empty_cols, additional_cols)
  row_data <- rowData(object)

  colnams <- colnames(row_data)
  msg <- c(msg, validate_cols(required_cols, colnams))

  if (all(non_empty_cols %in% colnams)) {
    df <- row_data[non_empty_cols]
    msg <- c(msg, validate_non_empty(df))
  }

  msg
}

#' @describeIn validate validates that the object contains `colData` with
#'   required columns.
#'
validate_col_data <- function(object) {
  msg <- NULL

  non_empty_cols <- .col_data_non_empty_cols
  additional_cols <- .col_data_additional_cols
  required_cols <- c(non_empty_cols, additional_cols)
  col_data <- colData(object)

  colnams <- colnames(col_data)
  msg <- c(msg, validate_cols(required_cols, colnams))

  if (all(non_empty_cols %in% colnams)) {
    df <- col_data[non_empty_cols]
    msg <- c(msg, validate_non_empty(df))
  }

  msg
}

#' @describeIn validate validates that the object contains row and column names.
#'
validate_names <- function(object) {
  msg <- NULL

  if (is.null(rownames(object))) {
    msg <- c(msg, "'object' must have rownames")
  }
  if (is.null(colnames(object))) {
    msg <- c(msg, "'object' must have colnames")
  }

  msg
}
