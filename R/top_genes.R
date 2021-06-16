#' Derivation of Top Genes
#'
#' This creates a [HermesDataTopGenes] object, which extends `data.frame`. It
#' contains two columns:
#' - `expression`: containing the statistic values calculated by `summary_fun` across columns.
#' - `name`: the gene names.
#'
#' The data frame is sorted in descending order of `expression` and only the top
#' entries according to the selection criteria are included.
#'
#' Note that exactly one of the arguments `n_top` and `min_threshold` must be
#' provided.
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
#' top_genes(object, n_top = NULL, min_threshold = 50000)
#' result <- top_genes(object, summary_fun = rowMax)
#'
top_genes <- function(object,
                      assay_name = "counts",
                      summary_fun = rowMeans,
                      n_top = if(is.null(min_threshold)) 10L else NULL,
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
    assert_that(
      is.number(min_threshold),
      min_threshold > 0,
      is.finite(min_threshold)
    )
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

# HermesDataTopGenes ----
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

# autoplot-HermesDataTopGenes ----

#' @describeIn top_genes Creates a bar plot from a [HermesDataTopGenes] object,
#'   where the y axis shows the expression statistics for each of the top genes
#'   on the x-axis.
#'
#' @param x_lab (`string`)\cr x-axis label.
#' @param y_lab (`string`)\cr y-axis label.
#' @param title (`string`)\cr plot title.
#'
#' @examples
#' autoplot(result, y_lab = "Maximum Count", title = "My top genes")
#'
setMethod(
  f = "autoplot",
  signature = c(object = "HermesDataTopGenes"),
  definition = function(object,
                        x_lab = "HGNC gene names",
                        y_lab = "Averaged Counts",
                        title = "Top most expressed genes") {
    assert_that(
      is.string(x_lab),
      is.string(y_lab),
      is.string(title)
    )

    df <- data.frame(object)

    ggplot(df) +
      geom_col(aes(y = .data$expression, x = .data$name)) +
      scale_x_discrete(name = x_lab) +
      scale_y_continuous(name = paste(y_lab, sep = ""))  +
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle(paste(title, sep = "")) +
      theme(plot.title = element_text(hjust = 0.5))
  }
)
