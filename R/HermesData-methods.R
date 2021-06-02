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
#' @param object (`AnyHermesData`)\cr object to filter.
#'
#' @return The filtered [AnyHermesData] object.
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
    assert_that(
      noNA(rowData(x)$LowExpressionFlag),
      noNA(colData(x)$LowDepthFlag),
      noNA(colData(x)$TechnicalFailureFlag)
    )
    subset(
      x,
      subset = (LowExpressionFlag == FALSE),
      select = (LowDepthFlag == FALSE & TechnicalFailureFlag == FALSE)
    )
  }
)
