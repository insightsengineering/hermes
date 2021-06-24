#' Example `ExpressionSet` Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This example data can be used to try out conversion of a [`Biobase::ExpressionSet`]
#' object into a [`HermesData`] object.
#'
#' @format A [`Biobase::ExpressionSet`] object with 20 samples covering 5085
#'   features (Entrez gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso summarized_experiment which contains similar data as
#'   [`SummarizedExperiment::SummarizedExperiment`].
"expression_set"

#' Example `SummarizedExperiment` Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This example [`SummarizedExperiment::SummarizedExperiment`] can be used to create a
#' [`HermesData`] object. It already contains the required columns in `rowData` and `colData`.
#'
#' @format A [SummarizedExperiment::SummarizedExperiment] object with 20 samples covering
#'   5085 features (Entrez gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso expression_set which contains similar data as [`Biobase::ExpressionSet`].
"summarized_experiment"

#' Example `MultiAssayExperiment` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`MultiAssayExperiment::MultiAssayExperiment`] can be used as test data.
#'
#' @format A [`MultiAssayExperiment::MultiAssayExperiment`] object with 3 separate SE objects. The first SE object
#'   contains 5 samples and covers 1000 features (Entrez gene IDs). The second SE object contains 9 samples with
#'   2500 features. The third SE object contains 6 samples with 1300 features.
#' @source This is an artificial dataset designed to resemble real data.
"multi_assay_experiment"
