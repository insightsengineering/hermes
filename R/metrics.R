# PCA ----
#' Principal Components Analysis Calculation Function
#' The `calc_pca()` function for principal components calculation function.
#' [HermesDataPca] class is an extention of [prcomp] class to enable use of plot
#' method for plotting PCA.
#' @aliases HermesDataPca
#' @exportClass HermesDataPca
#' 
#' @param object (`HermesData`) \cr input.
#' @param assay_name (`Character string`) \cr Indicating the name of the assay
#'   of interest, with possible options: "counts", "cpm", "tpm", "rpkm", "voom".
#'   Default assay is "counts".
#'
#' @return A list with class "prcomp" containing standard deviations of the
#'   principal components, rotation (matrix whose columns contain the
#'   eigenvectors), the rotated data and the cenetering and scaling used
#'   
#' @note Genes with constant value across all samples are excluded from the analysis.
#' @export calc_pca
#'
#' @importFrom S4Vectors isConstant
#' @importFrom stats prcomp
#' @examples
#' object <- HermesData(summarized_experiment)
#' result <- calc_pca(object)
#' summary(result)
#'

setOldClass("prcomp")
.HermesDataPca <- setClass(
  Class = "HermesDataPca",
  contains = "prcomp"
)

calc_pca <- function(object,
                    assay_name = "counts") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )

  x_samples <- assay(object, assay_name)
  x_genes <- t(x_samples)
  gene_is_constant <- apply(x_genes, MARGIN = 2L, FUN = isConstant)
  x_genes_remaining <- x_genes[, !gene_is_constant]

  pca <- stats::prcomp(
    x = x_genes_remaining,
    center = TRUE,
    scale = TRUE,
    tol = sqrt(.Machine$double.eps)
  )
  
  sample_pca <- .HermesDataPca(pca)
}
