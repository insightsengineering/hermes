# rbind ----

#' Row Binding of `AnyHermesData` Objects
#'
#' This method combines [`AnyHermesData`] objects with the same samples but different
#' features of interest (rows in assays).
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::rbind,SummarizedExperiment-method()]. When binding a
#'   [`AnyHermesData`] object with a [`SummarizedExperiment::SummarizedExperiment`]
#'   object, then the result will be a
#'   [`SummarizedExperiment::SummarizedExperiment`] object (the more general
#'   class).
#'
#' @name rbind
#'
#' @param ... (`AnyHermesData`)\cr objects to row bind.
#'
#' @return The combined [`AnyHermesData`] object.
#'
#' @examples
#' a <- b <- HermesData(summarized_experiment)
#' result <- rbind(a, b)
#' class(result)
#'
#' result2 <- rbind(summarized_experiment, b)
#' class(result2)
NULL

# cbind ----

#' Column Binding of `AnyHermesData` Objects
#'
#' This method combines [`AnyHermesData`] objects with the same ranges but different
#' samples (columns in assays).
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::cbind,SummarizedExperiment-method()]. When binding a
#'   [`AnyHermesData`] object with a [`SummarizedExperiment::SummarizedExperiment`]
#'   object, then the result will be a
#'   [`SummarizedExperiment::SummarizedExperiment`] object (the more general
#'   class).
#'
#' @name cbind
#'
#' @param ... (`AnyHermesData`)\cr objects to column bind.
#'
#' @return The combined [`AnyHermesData`] object.
#'
#' @examples
#' a <- b <- HermesData(summarized_experiment)
#' result <- cbind(a, b)
#' class(result)
#'
#' result2 <- cbind(summarized_experiment, b)
#' class(result2)
NULL

# metadata ----

#' Metadata Accessor and Setter
#'
#' These methods access or set the metadata in a [`AnyHermesData`] object.
#'
#' @note Note that this just inherits [S4Vectors::metadata,Annotated-method()].
#'
#' @name metadata
#'
#' @param x (`AnyHermesData`)\cr object to access the metadata from.
#'
#' @return The metadata which is a list.
#' @importFrom S4Vectors `metadata<-`
#' @importMethodsFrom S4Vectors metadata
#' @exportMethod metadata
#' @export `metadata<-`
#'
#' @examples
#' a <- HermesData(summarized_experiment)
#' metadata(a)
#' metadata(a) <- list(new = "my metadata")
#' metadata(a)
NULL

# counts ----

#' Counts Accessor and Setter
#'
#' These methods access and set the counts assay in a [`AnyHermesData`] object.
#'
#' @rdname counts
#' @aliases counts
#'
#' @param object (`AnyHermesData`)\cr object to access the counts from.
#' @param value (`matrix`)\cr what should the counts assay be replaced with.
#'
#' @return The counts assay.
#'
#' @importFrom BiocGenerics counts
#' @export
#'
#' @examples
#' a <- HermesData(summarized_experiment)
#' result <- counts(a)
#' class(result)
#' head(result)
setMethod(
  f = "counts",
  signature = "AnyHermesData",
  definition = function(object) {
    assay(object)
  }
)

#' @describeIn counts
#'
#' @importFrom BiocGenerics `counts<-`
#' @export
#'
#' @examples
#' counts(a) <- counts(a) + 100L
#' head(counts(a))
setReplaceMethod(
  f = "counts",
  signature = signature(object = "AnyHermesData", value = "matrix"),
  definition = function(object, value) {
    assay(object) <- value
    validObject(object)
    object
  }
)

# subset ----

#' Subsetting `AnyHermesData` Objects
#'
#' This method subsets [`AnyHermesData`] objects, based on expressions involving the
#' `rowData` columns and the `colData` columns.
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::subset,SummarizedExperiment-method()].
#'
#' @name subset
#'
#' @param x (`AnyHermesData`)\cr object to subset from.
#' @return The subsetted [`AnyHermesData`] object.
#'
#' @examples
#' a <- HermesData(summarized_experiment)
#' a
#' subset(a, subset = LowExpressionFlag, select = DISCSTUD == "N")
NULL

# filter ----

setGeneric("filter")

#' Filter `AnyHermesData` on Subset Passing Default QC Flags
#'
#' This filters a [`AnyHermesData`] object using the default QC flags. That is,
#' only genes without low expression (`LowExpressionFlag`) and samples
#' without low depth (`LowDepthFlag`) or technical failure (`TechnicalFailureFlag`)
#' remain in the returned filtered object.
#'
#' @rdname filter
#' @aliases filter
#'
#' @param x (`AnyHermesData`)\cr object to filter.
#'
#' @return The filtered [`AnyHermesData`] object.
#' @note The internal implementation cannot use the [subset()] method since that
#'   requires non-standard evaluation of arguments.
#'
#' @export
#'
#' @examples
#' a <- HermesData(summarized_experiment)
#' dim(a)
#' result <- filter(a)
#' dim(result)
setMethod(
  f = "filter",
  signature = signature(x = "AnyHermesData"),
  definition = function(x) {
    low_exp <- get_low_expression(x)
    low_depth <- get_low_depth(x)
    tech_fail <- get_tech_failure(x)
    assert_that(
      noNA(low_exp),
      noNA(low_depth),
      noNA(tech_fail),
      msg = "still NA in quality flags, please first run add_quality_flags() to fill them"
    )
    rows <- !low_exp
    cols <- !low_depth & !tech_fail
    x[rows, cols]
  }
)

test <- function(z, which = c("genes", "samples")) {
  which <- match.arg(which)
  if (which %in% c("genes")) {
    low_exp <- get_low_expression(x)
    assert_that(
      noNA(low_exp),
      msg = "still NA in quality flags, please first run add_quality_flags() to fill them"
    )
    rows <- !low_exp
    x[rows,]
  }
  if (which %in% c("samples")) {
    low_depth <- get_low_depth(x)
    tech_fail <- get_tech_failure(x)
    assert_that(
      noNA(low_depth),
      noNA(tech_fail),
      msg = "still NA in quality flags, please first run add_quality_flags() to fill them"
    )
    cols <- !low_depth & !tech_fail
    x[,cols]
  }
}
t1 <- test(x, which = "genes")
t2 <- test(x, which = "samples")
t3 <- test(x, which = c("genes", "samples"))
t4 <- test(x, which = "test")


# summary ----

#' @rdname summary
#' @aliases summary HermesDataSummary
#' @exportClass HermesDataSummary
.HermesDataSummary <- setClass( # nolint
  Class = "HermesDataSummary",
  slots = c(
    class_name = "character",
    n_genes = "integer",
    n_samples = "integer",
    additional_feature_cols = "character",
    additional_sample_cols = "character",
    no_qc_flags_filled = "logical",
    genes_fail = "character",
    samples_fail = "character",
    lib_sizes = "numeric",
    assay_names = "character"
  )
)

setGeneric("summary")

#' Summary Method for `AnyHermesData` Objects
#'
#' @describeIn summary A summary method for [`AnyHermesData`] object that
#'   creates a [`HermesDataSummary`] object.
#'
#' @param object (`AnyHermesData`)\cr input.
#'
#' @importFrom S4Vectors classNameForDisplay
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' object_summary <- summary(object)
#'
#' # We can access parts of this S4 object with the slot operator.
#' str(object_summary)
#' slotNames(object_summary)
#' object_summary@lib_sizes
setMethod(
  f = "summary",
  signature = c("AnyHermesData"),
  definition = function(object) {
    rd <- rowData(object)
    cd <- colData(object)
    additional_feature_cols <- setdiff(
      names(rd),
      union(.row_data_non_empty_cols, .row_data_additional_cols)
    )
    genes_fail <- rownames(object)[which(rd$LowExpressionFlag)]
    additional_sample_cols <- setdiff(
      names(cd),
      union(.col_data_non_empty_cols, .col_data_additional_cols)
    )
    samples_fail <- colnames(object)[which(cd$TechnicalFailureFlag | cd$LowDepthFlag)]
    no_qc_flags_filled <- is.null(metadata(object)$control_quality) &&
      all_na(rd$LowExpressionFlag) &&
      all_na(cd$TechnicalFailureFlag) &&
      all_na(cd$LowDepthFlag)

    .HermesDataSummary(
      class_name = S4Vectors::classNameForDisplay(object),
      n_genes = nrow(object),
      n_samples = ncol(object),
      additional_feature_cols = additional_feature_cols,
      additional_sample_cols = additional_sample_cols,
      no_qc_flags_filled = no_qc_flags_filled,
      genes_fail = genes_fail,
      samples_fail = samples_fail,
      lib_sizes = colSums(counts(object)),
      assay_names = assayNames(object)
    )
  }
)

#' @describeIn summary A show method prints summary description of [`HermesDataSummary`] object
#'   generated by the [summary()] method.
#'
#' @param object (`HermesDataSummary`) \cr result from the summary method applied to
#'   [`AnyHermesData`] object.
#'
#' @importFrom utils.nest cat_nl
#' @importFrom S4Vectors coolcat
#' @export
#'
#' @examples
#' # Just calling the summary method like this will use the `show()` method.
#' summary(object)
setMethod(
  f = "show",
  signature = c("HermesDataSummary"),
  definition = function(object) {
    cat_nl(
      object@class_name, "object with",
      object@n_samples, "samples of", object@n_genes, "genes."
    )
    sum_depth <- summary(object@lib_sizes)
    cat_nl(
      "- Library sizes across samples: ",
      "mean ", sum_depth["Mean"], ", median ", sum_depth["Median"], ", range ",
      sum_depth["Min."], " to ", sum_depth["Max."],
      sep = ""
    )
    S4Vectors::coolcat(
      "- Included assays (%d): %s\n",
      object@assay_names
    )
    if (length(object@additional_feature_cols)) {
      S4Vectors::coolcat(
        "- Additional feature columns (%d): %s\n",
        object@additional_feature_cols
      )
    }
    if (length(object@additional_sample_cols)) {
      S4Vectors::coolcat(
        "- Additional sample columns (%d): %s\n",
        object@additional_sample_cols
      )
    }
    if (object@no_qc_flags_filled) {
      cat_nl(
        "- QC flags still need to be added"
      )
    } else {
      S4Vectors::coolcat(
        "- Low expression genes (%d): %s\n",
        object@genes_fail
      )
      S4Vectors::coolcat(
        "- Samples with too low depth or technical failures (%d): %s\n",
        object@samples_fail
      )
    }
  }
)

# correlate ----

#' @name correlate
#' @title Generic Function for Correlation Calculations
#' @param object input of which the class will be used to decide the method.
#' @param ... additional arguments.
#' @return Corresponding object that contains the correlation results.
#' @seealso [pca_cor_samplevar] and [calc_cor] which are the methods included for this generic function.
setGeneric("correlate", function(object, ...) standardGeneric("correlate"))

# autoplot ----

setGeneric("autoplot")
