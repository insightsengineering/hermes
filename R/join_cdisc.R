#' Sample Variables with Selected Gene Information
#'
#' This obtains the sample variables of a `HermesData` object together
#' with selected gene information.
#'
#' @param object (`AnyHermesData`)\cr input experiment.
#' @param assay_name (`string`)\cr which assay to use.
#' @param genes (`GeneSpec`)\cr which genes or which gene signature should be extracted.
#'
#' @return The combined data set, where the additional attribute `gene_cols` contains
#'   the names of the columns obtained by extracting the `genes` information.
#'
#' @note The class of the returned data set will depend on the class of `colData`, so usually
#'   will be [`S4Vectors::DFrame`].
#' @export
#'
#' @examples
#' result <- col_data_with_genes(hermes_data, "counts", gene_spec("GeneID:1820"))
#' tail(names(result))
#' result$GeneID.1820
col_data_with_genes <- function(object,
                                assay_name,
                                genes) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_class(genes, "GeneSpec")

  col_data <- colData(object)
  assay_matrix <- assay(object, assay_name)
  gene_data <- genes$extract_data_frame(assay_matrix)

  assert_true(identical(
    rownames(col_data),
    rownames(gene_data)
  ))
  structure(
    cbind(
      col_data,
      gene_data
    ),
    gene_cols = names(gene_data)
  )
}
