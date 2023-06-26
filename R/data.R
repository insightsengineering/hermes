#' Example `ExpressionSet` Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This example data can be used to try out conversion of a [`Biobase::ExpressionSet`]
#' object into a [`HermesData`] object.
#'
#' @format A [`Biobase::ExpressionSet`] object with 20 samples covering 5085
#'   features (`Entrez` gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso
#'   - [SummarizedExperiment::makeSummarizedExperimentFromExpressionSet()] to convert into a
#'     [`SummarizedExperiment::SummarizedExperiment`].
#'   - [`summarized_experiment`] which contains similar data already as a
#'     [`SummarizedExperiment::SummarizedExperiment`].
"expression_set"

#' Example `SummarizedExperiment` Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This example [`SummarizedExperiment::SummarizedExperiment`] can be used to create a
#' [`HermesData`] object. It already contains the required columns in `rowData` and `colData`.
#'
#' @format A [SummarizedExperiment::SummarizedExperiment] object with 20 samples covering
#'   5085 features (`Entrez` gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso [`expression_set`] which contains similar data as a [`Biobase::ExpressionSet`].
"summarized_experiment"

#' Example `HermesData` Data
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This example [`hermes::HermesData`] is created from the underlying [`SummarizedExperiment::SummarizedExperiment`]
#' object by renaming descriptors to align with standard specification. It already
#' contains the required columns in `rowData` and `colData`.
#'
#' @format A [hermes::HermesData] object with 20 samples covering
#'   5085 features (`Entrez` gene IDs).
#' @source This is an artificial dataset designed to resemble real data.
#' @seealso [`summarized_experiment`] for the underlying [`SummarizedExperiment::SummarizedExperiment`]
#'   object.
"hermes_data"

#' Example `MultiAssayExperiment` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`MultiAssayExperiment::MultiAssayExperiment`] can be used as test data.
#'
#' @format A [`MultiAssayExperiment::MultiAssayExperiment`] object with 3 separate [`HermesData`]
#'   objects.
#'   - The first object contains 5 samples and covers 1000 features (`Entrez` gene IDs).
#'   - The second object contains 9 samples with 2500 features.
#'   - The third object contains 6 samples with 1300 features.
#' @source This is an artificial dataset designed to resemble real data.
#' @import MultiAssayExperiment
"multi_assay_experiment"
