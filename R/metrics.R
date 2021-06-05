#' Principal Components Analysis and Plot
#'
#' @param object (`HermesData`) \cr input.
#' @param assay_name 
#'
#' @return
#' @export
#'
#' @examples
calc_pca <- function(object,
                    assay_name = "counts") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )

  # Obtain a matrix where each column is a gene, and keep only non-constant genes.
  x_samples <- assay(object, assay_name)
  x_samples1 <- assay(object, "cpm")
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

object <- HermesData(summarized_experiment)
pca <- calc_pca(object)
summary(pca)