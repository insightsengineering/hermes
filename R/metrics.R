#' Principal Components Analysis and Plot
#'
#' @param object (`HermesData`) \cr input.
#' @param assay_name (`Character string`) \cr Indicating the name of the assay of interest, with possible options: "counts", "cpm", "tpm", "rpkm", "voom". Default assay is "counts".
#'
#' @return A list with class "prcomp" containing standard deviations of the principal components, rotation (matrix of variable loadings), and 
#' 
#' @note Genes with constant value across all samples are excluded from the analysis.
#' @export
#'
#' @importFrom stats prcomp
#' @examples
#' object <- HermesData(summarized_experiment)
#' pca <- calc_pca(object)
#' summary(pca)
#' 
calc_pca <- function(object,
                    assay_name = "counts") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )

  # Obtain a matrix where each column is a gene, and keep only non-constant genes.
  x_samples <- assay(object, assay_name)
  x_genes <- t(x_samples)
  gene_is_constant <- apply(x_genes, MARGIN = 2L, FUN = isConstant)
  x_genes_remaining <- x_genes[, !gene_is_constant]

  stats::prcomp(
    x = x_genes_remaining,
    center = TRUE,
    scale = TRUE,
    tol = sqrt(.Machine$double.eps)
  )
}

## Difficult to test other assays besides "counts" without the normalization method defined to be able to easily create a 
##### normalized HermesData object
object <- HermesData(summarized_experiment)
h_norm <- normalize(object) ## NEED TO SET METHOD for normalization first to be able to use this function for other assays besides "counts"
assayNames(h_norm)
pca <- calc_pca(h_norm, assay_name = "cpm")
summary(pca)
