#' Example `ExpressionSet` Data
#'
#' This example data can be used to try out conversion of a [Biobase::ExpressionSet] 
#' object into a [HermesData] object. Note that this data does not yet have the
#' required columns in the `featureData` and `phenoData`.
#'
#' @format A [Biobase::ExpressionSet] object with 20 samples covering 5085
#'   features (Entrez gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso summarized_experiment which contains similar data as 
#'   [SummarizedExperiment::SummarizedExperiment].
"expression_set"

#' Example `SummarizedExperiment` Data
#'
#' This example [SummarizedExperiment::SummarizedExperiment] can be used to create a
#' [HermesData] object. It already contains the required columns in `rowData` and `colData`.
#' 
#' @format A [SummarizedExperiment::SummarizedExperiment] object with 20 samples covering
#'   5085 features (Entrez gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso expression_set which contains similar data as [Biobase::ExpressionSet].
"summarized_experiment"
