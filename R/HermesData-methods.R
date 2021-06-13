# rbind ----

#' Row Binding of HermesData Objects
#'
#' This method combines HermesData objects with the same samples but different
#' features of interest (rows in assays).
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::rbind,SummarizedExperiment-method()]. When binding a
#'   [HermesData] object with a [SummarizedExperiment::SummarizedExperiment]
#'   object, then the result will be a
#'   [SummarizedExperiment::SummarizedExperiment] object (the more general
#'   class).
#'
#' @name rbind
#'
#' @param ... (`AnyHermesData`)\cr objects to row bind.
#'
#' @return The combined [AnyHermesData] object.
#'
#' @examples
#' a <- b <- HermesData(summarized_experiment)
#' result <- rbind(a, b)
#' class(result)
#'
#' result2 <- rbind(summarized_experiment, b)
#' class(result2)
#' 
NULL

# cbind ----

#' Column Binding of HermesData Objects
#'
#' This method combines HermesData objects with the same ranges but different
#' samples (columns in assays).
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::cbind,SummarizedExperiment-method()]. When binding a
#'   [HermesData] object with a [SummarizedExperiment::SummarizedExperiment]
#'   object, then the result will be a
#'   [SummarizedExperiment::SummarizedExperiment] object (the more general
#'   class).
#'
#' @name cbind
#'
#' @param ... (`AnyHermesData`)\cr objects to column bind.
#'
#' @return The combined [AnyHermesData] object.
#'
#' @examples
#' a <- b <- HermesData(summarized_experiment)
#' result <- cbind(a, b)
#' class(result)
#'
#' result2 <- cbind(summarized_experiment, b)
#' class(result2)
#' 
NULL

# metadata ----

#' Metadata Accessor and Setter
#'
#' These methods access or set the metadata in a [AnyHermesData] object.
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
#' 
NULL

# counts ----

#' Counts Accessor and Setter
#'
#' These methods access and set the counts assay in a [HermesData] object.
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
#' 
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
#' 
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

#' Subsetting HermesData Objects
#'
#' This method subsets HermesData objects, based on expressions involving the
#' `rowData` columns and the `colData` columns.
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::subset,SummarizedExperiment-method()].
#'
#' @name subset
#' 
#' @param x (`AnyHermesData`)\cr object to subset from.
#' @return The subsetted [AnyHermesData] object.
#'
#' @examples
#' a <- HermesData(summarized_experiment)
#' a
#' subset(a, subset = LowExpressionFlag, select = DISCSTUD == "N")
#' 
NULL

# filter ----

setGeneric("filter")

#' Filter HermesData on Subset Passing Default QC Flags
#'
#' This is a short cut to subset a [AnyHermesData] object for the default QC flag, 
#' i.e. only features without low expression (`LowExpressionFlag`) and without samples
#' without low depth (`LowDepthFlag`) or technical failure (`TechnicalFailureFlag`)
#' remain in the returend subset.
#' 
#' @rdname filter
#' @aliases filter
#' 
#' @param x (`AnyHermesData`)\cr object to filter.
#'
#' @return The filtered [AnyHermesData] object.
#' @note Internal implementation cannot use the [subset()] method since that
#'   requires non-standard evaluation of arguments.
#'  
#' @export
#' 
#' @examples 
#' a <- HermesData(summarized_experiment)
#' dim(a)
#' result <- filter(a)
#' dim(result)
#' 
setMethod(
  f = "filter",
  signature = signature(x = "AnyHermesData"),
  definition = function(x) {
    low_exp <- rowData(x)$LowExpressionFlag
    low_depth <- colData(x)$LowDepthFlag
    tech_fail <- colData(x)$TechnicalFailureFlag
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

# Summary ----
#' @rdname summary
#' @aliases HermesDataSummary
#' @exportClass HermesDataSummary
.HermesDataSummary <- setClass(
  Class = "HermesDataSummary",
  slots = c(
    class_name = "character",
    n_genes = "integer",
    n_samples = "integer",
    additional_feature_cols = "character",
    additional_sample_cols = "character",
    no_qc_flags_filled = "logical",
    genes_fail= "character",
    samples_fail = "character",
    lib_sizes = "numeric",
    assay_names = "character"
  )
)

setGeneric("summary")
#' Summary Function for `AnyHermesData`
#'
#' @describeIn summary A summary method for [AnyHermesData] object that creates a [HermesDataSummary] object.
#'
#' @param object (`AnyHermesData`) \cr input
#'
#' @export
#'
#' @importFrom S4Vectors classNameForDisplay
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' summary(object)
#' str(summary(object))
#' 
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

#' @describeIn summary A show method prints summary description of [HermesDataSummary] object generated by
#'   [summary()] function.
#'
#' @param object (`HermesDataSummary`) \cr input
#'
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' result <- summary(object)
#' show(result)
#' result
#'
setMethod(
  f = "show",
  signature = c("HermesDataSummary"),
  definition = function(object) {
    if (object@no_qc_flags_filled == "FALSE") {
      cat(
        object@class_name, "object with",
        object@n_samples, "samples of", object@n_genes, "genes for", object@assay_names,
        "assay,", length(object@genes_fail), "genes flagged as low expression genes,", length(object@samples_fail),
        "samples flagged as either low depth or technical failure samples. In addition to required feature",
        "and sample data columns in a", object@class_name, "object, there are", length(object@additional_feature_cols),
        "additional feature data columns and", length(object@additional_sample_cols),
        "additional sample data columns. There is are no missing QC flags."
      )
    } else {
      cat(
        object@class_name, "object with",
        object@n_samples, "samples of", object@n_genes, "genes for", object@assay_names,
        "assay,", length(object@genes_fail), "genes flagged as low expression genes,", length(object@samples_fail),
        "samples flagged as either low depth or technical failure samples. In addition to required feature",
        "and sample data columns in a", object@class_name, "object, there are", length(object@additional_feature_cols),
        "additional feature data columns and", length(object@additional_sample_cols),
        "additional sample data columns. At least one QC flag has missing values."
      )
    }
  }
)