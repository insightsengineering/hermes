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
#' @param ... (`HermesData`)\cr objects to row bind.
#'
#' @return The combined [HermesData] object.
#' @importMethodsFrom SummarizedExperiment rbind
#'
#' @examples
#' a <- b <- hermes:::.HermesData(summarized_experiment)
#' result <- rbind(a, b)
#' class(result)
#'
#' result2 <- rbind(summarized_experiment, b)
#' class(result2)
#' 
NULL

#' Metadata Accessor
#'
#' This method accesses the metadata in a [HermesData] object.
#' 
#' @note Note that this just inherits [S4Vectors::metadata,Annotated-method()].
#' 
#' @name metadata
#' 
#' @param x (`HermesData`)\cr object to access the metadata from.
#'
#' @return The metadata which is a list.
#' @importMethodsFrom S4Vectors metadata
#' 
#' @examples 
#' a <- hermes:::.HermesData(summarized_experiment)
#' metadata(a)
#' metadata(a) <- list(new = "my metadata")
#' metadata(a)
#' 
NULL
