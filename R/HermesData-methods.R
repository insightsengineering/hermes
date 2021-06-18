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

#' Filter `AnyHermesData` on Subset Passing Default QC Flags
#'
#' This filters a [`AnyHermesData`] object using the default QC flags. That is,
#' only genes without low expression (`LowExpressionFlag`) and samples
#' without low depth (`LowDepthFlag`) or technical failure (`TechnicalFailureFlag`)
#' remain in the returned filtered object.
#'
#' @param x (`AnyHermesData`)\cr object to filter.
#'
#' @return The filtered [`AnyHermesData`] object.
#' @note The internal implementation cannot use the [subset()] method since that
#'   requires non-standard evaluation of arguments.
#'
#' @export
setGeneric("filter", function(x, ...) standardGeneric("filter"))

#' @rdname filter
#' @param what (`vector`) \cr specify `genes` and / or `samples` to apply filter
#' @export
#' @examples
#' a <- HermesData(summarized_experiment)
#' dim(a)
#' # Filter genes and samples on default QC flags
#' result <- filter(a)
#' dim(result)
#' # Filter only genes without low expression
#' result <- filter(a, what = "genes")
setMethod(
  f = "filter",
  signature = signature(x = "AnyHermesData"),
  definition = function(x, what = c("genes", "samples")) {
    low_exp <- get_low_expression(x)
    low_depth <- get_low_depth(x)
    tech_fail <- get_tech_failure(x)
    what <- match.arg(what, c("genes", "samples"), several.ok = TRUE)
    assert_that(
      noNA(low_exp),
      noNA(low_depth),
      noNA(tech_fail),
      msg = "still NA in quality flags, please first run add_quality_flags() to fill them"
    )
    rows <- if ("genes" %in% what) {
      !low_exp
    } else {
      seq_along(low_exp)
    }
    cols <- if ("samples" %in% what) {
      !low_depth & !tech_fail
    } else {
      seq_along(low_depth)
    }
    x[rows, cols]
  }
)

#' Extra Variable Names Accessor Methods
#'
#' The methods access the names of the variables in `colData()` and `rowData()` of
#' the object which are not required by design. So these can be additional sample or
#' patient characteristics, or gene characteristics.
#'
#' @name extra_data_names
#'
#' @param x (`AnyHermesData`)\cr object.
#' @param ... not used.
#'
#' @return The character vector with the additional variable names in either
#'   `colData()` or `rowData()`.
#'
#' @examples
#' object <- HermesData(summarized_experiment)
NULL

# extraColDataNames ----

#' @rdname extra_data_names
#' @export
setGeneric("extraColDataNames", function(x, ...) standardGeneric("extraColDataNames"))

#' @rdname extra_data_names
#' @export
#' @examples
#' extraColDataNames(object)
setMethod(
  f = "extraColDataNames",
  signature = c(x = "AnyHermesData"),
  definition = function(x, ...) {
    cd_names <- colnames(colData(x))
    cd_req_names <- union(.col_data_non_empty_cols, .col_data_additional_cols)
    setdiff(cd_names, cd_req_names)
  }
)

# extraRowDataNames ----

#' @rdname extra_data_names
#' @export
setGeneric("extraRowDataNames", function(x, ...) standardGeneric("extraRowDataNames"))

#' @rdname extra_data_names
#' @export
#' @examples
#' extraRowDataNames(object)
setMethod(
  f = "extraRowDataNames",
  signature = c(x = "AnyHermesData"),
  definition = function(x, ...) {
    rd_names <- colnames(rowData(x))
    rd_req_names <- union(.row_data_non_empty_cols, .row_data_additional_cols)
    setdiff(rd_names, rd_req_names)
  }
)

# summary ----

setGeneric("summary")

#' @rdname summary
#' @aliases summary HermesDataSummary
#' @exportClass HermesDataSummary
.HermesDataSummary <- setClass( # nolint
  Class = "HermesDataSummary",
  slots = c(
    class_name = "character",
    n_genes = "integer",
    n_samples = "integer",
    additional_gene_info = "character",
    additional_sample_info = "character",
    no_qc_flags_filled = "logical",
    genes_fail = "character",
    samples_fail = "character",
    lib_sizes = "numeric",
    assay_names = "character"
  )
)

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
    low_expression <- get_low_expression(object)
    genes_fail <- rownames(object)[low_expression]

    tech_failure <- get_tech_failure(object)
    low_depth <- get_low_depth(object)
    samples_fail <- colnames(object)[tech_failure | low_depth]

    no_qc_flags_filled <- is.null(metadata(object)$control_quality) &&
      all_na(low_expression) &&
      all_na(tech_failure) &&
      all_na(low_depth)

    .HermesDataSummary(
      class_name = S4Vectors::classNameForDisplay(object),
      n_genes = nrow(object),
      n_samples = ncol(object),
      additional_gene_info = extraRowDataNames(object),
      additional_sample_info = extraColDataNames(object),
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
    if (length(object@additional_gene_info)) {
      S4Vectors::coolcat(
        "- Additional gene information (%d): %s\n",
        object@additional_gene_info
      )
    }
    if (length(object@additional_sample_info)) {
      S4Vectors::coolcat(
        "- Additional sample information (%d): %s\n",
        object@additional_sample_info
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

# show ----

.show.AnyHermesData <- function(object) { # nolint
  cat_nl(
    "class:",
    S4Vectors::classNameForDisplay(object)
  )
  S4Vectors::coolcat(
    "assays(%d): %s\n",
    assayNames(object)
  )
  S4Vectors::coolcat(
    "genes(%d): %s\n",
    rownames(object)
  )
  S4Vectors::coolcat(
    "additional gene information(%d): %s\n",
    extraRowDataNames(object)
  )
  coolcat(
    "samples(%d): %s\n",
    colnames(object)
  )
  S4Vectors::coolcat(
    "additional sample information(%d): %s\n",
    extraColDataNames(object)
  )
}

#' Show Method for `AnyHermesData` Objects
#'
#' A show method that displays high-level information of [`AnyHermesData`] objects.
#'
#' @rdname show
#' @aliases show
#'
#' @param object (`AnyHermesData`)\cr input.
#'
#' @note The same method is used for both [`HermesData`] and [`RangedHermesData`]
#'   objects. We need to define this separately to have this method used instead of
#'   the one inherited from [`SummarizedExperiment::SummarizedExperiment`].
#'
#' @importFrom utils.nest cat_nl
#' @importFrom S4Vectors classNameForDisplay coolcat
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' object
setMethod(
  f = "show",
  signature = "HermesData",
  definition = .show.AnyHermesData
)

#' @rdname show
setMethod(
  f = "show",
  signature = "RangedHermesData",
  definition = .show.AnyHermesData
)

# correlate ----

#' @name correlate
#' @title Generic Function for Correlation Calculations
#' @param object input of which the class will be used to decide the method.
#' @param ... additional arguments.
#' @return Corresponding object that contains the correlation results.
#' @seealso [pca_cor_samplevar] and [calc_cor] which are the methods included for this generic function.
#' @export
setGeneric("correlate", function(object, ...) standardGeneric("correlate"))

# autoplot ----

setGeneric("autoplot")
