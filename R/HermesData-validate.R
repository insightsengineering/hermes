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
.row_data_cols <- c(
  "symbol",
  "desc",
  "chromosome",
  "size",
  "low_expression_flag"
)
.col_data_cols <- c(
  "low_depth_flag",
  "tech_failure_flag"
)

#' @describeIn validate validates that the first assay is `counts` containing
#'   non-missing, integer, non-negative values.
#' @param object (`SummarizedExperiment`)\cr object to validate.
#'
validate_counts <- function(object) {
  nams <- assayNames(object)
  if (!("counts" %in% nams)) {
    return("no 'counts' assay found, consider using rename() to change assay name")
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
  if (any(counts < 0, na.rm = TRUE)) {
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

#' @describeIn validate validates that the object contains `rowData` with
#'   required columns.
validate_row_data <- function(object) {
  required_cols <- .row_data_cols
  actual_cols <- colnames(rowData(object))
  validate_cols(required_cols, actual_cols)
}

#' @describeIn validate validates that the object contains `colData` with
#'   required columns.
validate_col_data <- function(object) {
  required_cols <- .col_data_cols
  actual_cols <- colnames(colData(object))
  validate_cols(required_cols, actual_cols)
}

#' @describeIn validate validates that the object contains row and column names.
validate_names <- function(object) {
  msg <- NULL

  if (is.null(rownames(object))) {
    msg <- c(msg, "'object' must have rownames")
  }
  if (is.null(colnames(object))) {
    msg <- c(msg, "'object' must have colnames")
  }
  if (any(duplicated(rownames(object)))) {
    msg <- c(msg, "'object' must have unique rownames")
  }
  if (any(duplicated(colnames(object)))) {
    msg <- c(msg, "'object' must have unique colnames")
  }

  msg
}

#' @describeIn validate validates that the object prefix is a string
#'   and only contains alphabetic characters.
validate_prefix <- function(object) {
  prefix <- object@prefix
  msg <- NULL

  if (!is.string(prefix)) {
    msg <- c(msg, "'prefix' must be string")
  } else {
    if (grepl("[^[:alpha:]]", prefix)) {
      msg <- c(msg, "'prefix' can only consist of alphabetic characters")
    }
    gene_ids <- rownames(object)
    if (!all(grepl(paste0("^", prefix), gene_ids))) {
      msg <- c(msg, "'prefix' does not match at least one gene ID")
    }
  }
  msg
}
