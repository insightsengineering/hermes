#' Principal Components Analysis Calculation
#' 
#' Perform principal components analysis of the gene count vectors across all
#' samples.
#' 
#' - PCA should be performed after filtering out low quality genes and samples and
#'   normalization.
#' - In addition, genes with constant counts across all samples are excluded from 
#'   the analysis internally.
#' - Centering and scaling is applied internally.
#' - Plots can be obtained with the [ggplot2::autoplot()] function
#'   with the corresponding method from the `ggfortify` package to plot the
#'   results of a principal components analysis saved in a [HermesDataPca]
#'   object. See [ggfortify::autoplot.prcomp()] for details.
#' 
#' @param object (`AnyHermesData`) \cr input.
#' @param assay_name (`Character string`) \cr Indicating the name of the assay
#'   of interest, with possible options: "counts", "cpm", "tpm", "rpkm", "voom".
#'   Default assay is "counts".
#'
#' @return A [HermesDataPca] object which is an extension of the [stats::prcomp] class.
#'
#' @importFrom S4Vectors isConstant
#' @importFrom stats prcomp
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment) %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#' result <- calc_pca(object, assay_name = "tpm")
#' summary(result)
#' 
#' autoplot(result)
#' autoplot(result, x = 2, y = 3)
#' autoplot(result, variance_percentage = FALSE)
#' autoplot(result, label = TRUE)
#'
calc_pca <- function(object,
                     assay_name = "counts") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )
  
  x_samples <- assay(object, assay_name)
  x_genes <- t(x_samples)
  gene_is_constant <- apply(x_genes, MARGIN = 2L, FUN = S4Vectors::isConstant)
  x_genes_remaining <- x_genes[, !gene_is_constant]
  
  pca <- stats::prcomp(
    x = x_genes_remaining,
    center = TRUE,
    scale = TRUE,
    tol = sqrt(.Machine$double.eps)
  )
  
  .HermesDataPca(pca)
}

setOldClass("prcomp")

#' @rdname calc_pca
#' @aliases HermesDataPca
#' @exportClass HermesDataPca
.HermesDataPca <- setClass(
  Class = "HermesDataPca",
  contains = "prcomp"
)
