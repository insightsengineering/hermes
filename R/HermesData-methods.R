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
#' @export
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
