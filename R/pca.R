#' Principal Components Analysis Calculation
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `calc_pca()` function performs principal components analysis of the gene count
#' vectors across all samples.
#'
#' A corresponding `autoplot()` method then can visualize the results.
#'
#' @details
#' - PCA should be performed after filtering out low quality genes and samples, as well as
#'   normalization of counts.
#' - In addition, genes with constant counts across all samples are excluded from
#'   the analysis internally in `calc_pca()`. Centering and scaling is also applied internally.
#' - Plots can be obtained with the [ggplot2::autoplot()] function
#'   with the corresponding method from the `ggfortify` package to plot the
#'   results of a principal components analysis saved in a [`HermesDataPca`]
#'   object. See [ggfortify::autoplot.prcomp()] for details.
#'
#' @param object (`AnyHermesData`) \cr input.
#' @param assay_name (`string`) \cr name of the assay to use.
#' @param n_top (`count` or `NULL`)\cr filter criteria based on number of genes with maximum variance.
#'
#' @return A [HermesDataPca] object which is an extension of the [stats::prcomp] class.
#'
#' @seealso Afterwards correlations between principal components
#'   and sample variables can be calculated, see [`pca_cor_samplevar`].
#'
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment) %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#'
#' result <- calc_pca(object, assay_name = "tpm")
#' summary(result)
#'
#' result1 <- calc_pca(object, assay_name = "tpm", n_top = 500)
#' summary(result1)
#'
#' # Plot the results.
#' autoplot(result)
#' autoplot(result, x = 2, y = 3)
#' autoplot(result, variance_percentage = FALSE)
#' autoplot(result, label = TRUE, label.repel = TRUE)
calc_pca <- function(object,
                     assay_name = "counts",
                     n_top = NULL) {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name),
    is.numeric(n_top) || is.null(n_top)
  )

  x_samples_filtered <- if (is.null(n_top)) {
    object
  } else {
    top_var <- top_genes(
      object = object,
      assay_name = assay_name,
      summary_fun = matrixStats::rowVars,
      n_top = n_top
    )
    is_top_gene <- rownames(object) %in% as.character(top_var$name)
    object[is_top_gene, ]
  }

  x_samples <- assay(x_samples_filtered, assay_name)
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

# HermesDataPca ----

#' @rdname calc_pca
#' @aliases HermesDataPca
#' @exportClass HermesDataPca
.HermesDataPca <- setClass( # nolint
  Class = "HermesDataPca",
  contains = "prcomp"
)
