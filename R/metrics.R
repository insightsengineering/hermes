# PCA ----

setOldClass("prcomp")

#' @rdname calc_pca
.HermesDataPca <- setClass(
  Class = "HermesDataPca",
  contains = "prcomp"
)

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

# Correlation of samples ----
        
#' @rdname calc_cor   
.HermesDataCor <- setClass(
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)

#' Correlation between Samples of HermesData
#' 
#' This calculates the correlation matrix between the sample vectors of counts from 
#' a specified assay.
#' 
#' @aliases HermesDataCor
#' @exportClass HermesDataCor
#' 
#' @param object (`AnyHermesData`)\cr object to calculate the correlation.
#' @param assay_name (`string`)\cr the assay name where the counts are located in.
#' @param method (`string`)\cr the correlation method, see [stats::cor()] for details.
#'   
#' @return A [HermesDataCor] object which is an extension of a [matrix] with
#'   additional quality flags in the slot `flag_data`: This contains the 
#'   `TechnicalFailureFlag` and `LowDepthFlag` columns describing the original 
#'   input samples.
#' 
#' @importFrom stats cor
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' calc_cor(object)
#' calc_cor(object, method = "spearman")
#'               
calc_cor <- function(object,
                     assay_name = "counts", 
                     method = "pearson") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )
  
  chosen_assay <- assay(object, assay_name)
  sample_cor_matrix <- stats::cor(chosen_assay, method = method)
  
  .HermesDataCor(
    sample_cor_matrix,
    flag_data = colData(object)[, c("TechnicalFailureFlag", "LowDepthFlag")]
  )
}

# Top Genes ----

#' @rdname top_genes
#' @aliases HermesDataTopGenes
#' @exportClass HermesDataTopGenes
.HermesDataTopGenes <- setClass(
  Class = "HermesDataTopGenes",
  contains = "data.frame",
  slots = c(
    summary_fun_name = "character",
    assay_name = "character"
  )
)

#' Derivation of Top Genes 
#' 
#' This creates a [HermesDataTopGenes] object, which extends `data.frame`. It
#' contains two columns: `expression` containing the statistic values calculated
#' by `summary_fun` across columns, and `name` with the names of the rows. The
#' data frame is sorted in descending order of `expression` and only the top
#' entries according to the selection criteria are included.
#'
#' Note that exactly one of the arguments `n_top` and `min_threshold` must be
#' provided.
#'
#' @name top_genes
#' 
#' @param object (`AnyHermedData`)\cr input.
#' @param assay_name (`string`)\cr name of the assay to use for the sorting of genes.
#' @param summary_fun (`function`)\cr summary statistics function to apply to the assay resulting in 
#'   a numeric vector with one value per gene.
#' @param n_top (`count` or `NULL`)\cr selection criteria based on number of entries.
#' @param min_threshold (`number` or `NULL` )\cr selection criteria based on a minimum 
#'   summary statistics threshold.
#'
#' @return A [HermesDataTopGenes] object.
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' top_genes(object)
#' top_genes(result, n_top = NULL, min_threshold = 50000)
#' top_genes(result, summary_fun = rowMax)
#' 
top_genes <- function(object,
                      assay_name = "counts",
                      summary_fun = rowMeans,
                      n_top = 10L,
                      min_threshold = NULL) {
  assert_that(
    is_hermes_data(object),
    is.function(summary_fun),
    is.string(assay_name),
    one_provided(n_top, min_threshold)
  )
  
  x <- assay(object, assay_name)
  stat_values <- summary_fun(x)
  assert_that(
    is.numeric(stat_values), 
    identical(length(stat_values), nrow(object)),
    is.vector(stat_values)
  )
  
  df <- data.frame(expression = stat_values)
  df <- df[order(df$expression, decreasing = TRUE), , drop = FALSE]
  row_names <- rownames(object)
  df$name <- factor(
    row_names,
    levels = row_names
  )
  
  keep_row <- if (!is.null(min_threshold)) {
    assert_that(is.number(min_threshold))
    df$expression >= min_threshold
  } else {
    assert_that(is.count(n_top))
    seq_len(n_top)
  }
  df <- df[keep_row, ]
  
  .HermesDataTopGenes(
    df,
    summary_fun_name = deparse(substitute(summary_fun)),
    assay_name = assay_name
  )
}
