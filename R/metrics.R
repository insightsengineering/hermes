# PCA ----

setOldClass("prcomp")

#' @rdname calc_pca
.HermesDataPca <- setClass(
  Class = "HermesDataPca",
  contains = "prcomp"
)

#' Principal Components Analysis Calculation Function
#' 
#' Perform principal components analysis of the gene count vectors across all
#' samples.
#' 
#' - PCA should be performed after filtering out low quality genes and samples and
#'   normalization.
#' - In addition, genes with constant counts across all samples are excluded from 
#'   the analysis internally.
#' - Centering and scaling is applied internally.
#' 
#' @aliases HermesDataPca
#' @exportClass HermesDataPca
#' 
#' @param object (`AnyHermesData`) \cr input.
#' @param assay_name (`Character string`) \cr Indicating the name of the assay
#'   of interest, with possible options: "counts", "cpm", "tpm", "rpkm", "voom".
#'   Default assay is "counts".
#'
#' @return A [HermesDataPca] object which is an extension of the [stats::prcomp] class
#'   to enable use of plot method for plotting PCA.
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

# HermesDataCor ----

#' HermesData Correlation
#' 
#' The [HermesDataCor] class is an extension of a [matrix] with additional quality flags.
#' 
#' @note The `cor()` function will return an object of [HermesDataCor] class. 
#' @aliases HermesDataCor
#' @exportClass HermesDataCor
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' cor(x = object)
#' # which will return an object of HermesDataCor. 
#'          
.HermesDataCor <- setClass(
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)

# cor ----

setGeneric("cor")

#' Correlation Function
#' 
#' @rdname cor
#' @aliases cor
#' 
#' @param x (`AnyHermesData`)\cr object to calculate the correlation.
#' @param y (`string`)\cr the assay name where the counts are located in x (AnyHermesData object).
#' @param use not used.
#' @param method (`string`)\cr the correlation coefficient (or covariance) to be computed, either "pearson", "kendall", or "spearman". 
#'   
#' @return A [HermesDataCor] object with calculated correlations and QC flags (technical failure and low depth).
#' 
#' @importFrom stats cor
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' cor(x = object)
#' cor(x = object, method = "spearman")
#'               
setMethod(
  f = "cor",
  signature = signature(x = "AnyHermesData"),
  definition = function(x, y = "counts", use = NULL, method = "pearson") {
    assert_that(
      is.string(y),
      is.null(use)
    )
    
    chosen_assay <- assay(x, y)
    sample_cor_matrix <- stats::cor(chosen_assay, method = method)
    
    .HermesDataCor(
      sample_cor_matrix,
      flag_data = colData(x)[, c("TechnicalFailureFlag", "LowDepthFlag")]
    )
  }
)
