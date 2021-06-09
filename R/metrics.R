#' Derivation of Top Genes 
#' 
#' This creates a `data.frame` containing specified summary across columns and
#' the names of the rows. The data frame is sorted in descending order and the
#' top entries matching the selection criteria are selected.
#' Note that exactly one of the settings `n_top` and `min_threshold` must be provided.
#'
#' @param object (`AnyHermedData`)\cr input.
#' @param assay_name (`string`)\cr name of the assay to use for the sorting of genes.
#' @param summary_fun (`function`)\cr summary statistics function to apply to the assay resulting in 
#'   a vector with one number per gene.
#' @param n_top (`count` or `NULL`)\cr selection criteria based on number of entries.
#' @param min_threshold (`number` or `NULL` )\cr selection criteria based on a minimum 
#'   summary statistics threshold.
#'
#' @return `HermesDataTopGenes`\cr class object.
#' @export
#'
#' @examples
#' result <- HermesData(summarized_experiment)
#' df <- top_genes(object = result)
#' 
.HermesDataTopGenes <- setClass(
  Class = "HermesDataTopGenes",
  contains = "data.frame",
  slots = c(
    summary_fun_name = "character",
    assay_name = "character"
  )
)

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
  
  average_expression <- summary_fun(assay(
    object,
    assay_name
  ))
  
  average_expression <- data.frame(expression = average_expression)
  #colnames(average_expression) <- c("expression")
  average_expression <- average_expression[order(average_expression$expression, decreasing = TRUE), , drop = FALSE]
  
  row_names <- rownames(average_expression)
  average_expression$name <- factor(
    row_names,
    levels = row_names
  )
  
  keep_row <- if (!is.null(min_threshold)) {
    assert_that(is.number(min_threshold))
    average_expression$expression >= min_threshold
  } else {
    assert_that(is.count(n_top))
    seq_len(n_top)
  }
  
  df <- average_expression[keep_row, ]
  .HermesDataTopGenes(
    df,
    summary_fun_name = deparse(substitute(summary_fun)),
    assay_name = assay_name
  )
}
