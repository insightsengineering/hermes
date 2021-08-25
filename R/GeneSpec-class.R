#' R6 Class Representing a Gene (Signature) Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A `GeneSpec` consists of the gene names, the summary function and the name
#' of the summary function.
#'
#' @export
#'
#' @examples
#' # Minimal specification if only one gene is used.
#' x_spec <- gene_spec("GeneID:1820")
#'
#' # Using multiple genes with a signature.
#' x_spec <- gene_spec(c("GeneID:1820", "GeneID:52"), fun = colMeans)
#' x_spec$returns_vector()
#' x_spec$get_genes()
#' x_spec$get_label()
#'
#' # Using multiple genes without a signature.
#' x_spec <- gene_spec(c("GeneID:1820", "GeneID:52"))
#' x_spec$returns_vector()
#'
#' # Use the gene specification to extract genes from a matrix.
#' mat <- matrix(
#'   data = rpois(15, 10),
#'   nrow = 3, ncol = 5,
#'   dimnames = list(c("GeneID:1820", "GeneID:52", "GeneID:523"), NULL)
#' )
#' x_spec$extract(mat)
GeneSpec <- R6::R6Class(
  "GeneSpec",
  public = list(
    #' @description Creates a new [`GeneSpec`] object.
    #' @param genes (`character` or `NULL`)\cr the gene IDs.
    #' @param fun (`function` or `NULL`)\cr summary function. If `NULL` is
    #'   used then multiple genes are not summarized but returned as a matrix from the
    #'   `extract` method.
    #' @param fun_name (`string`)\cr name of the summary function.
    #' @return A new [`GeneSpec`] object.
    initialize = function(genes,
                          fun,
                          fun_name) {
      assert_character(genes, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
      assert_function(fun, null.ok = TRUE)
      assert_string(fun_name, min.chars = 1L)

      private$genes <- genes
      private$fun <- fun
      private$fun_name <- fun_name
    },
    #' @description Returns the genes.
    get_genes = function() {
      private$genes
    },
    #' @description Predicate whether the extract returns a vector or not.
    returns_vector = function() {
      identical(length(private$genes), 1L) || is.function(private$fun)
    },
    #' @description Returns a string which can be used e.g. for plot labels.
    get_label = function() {
      assert_true(self$returns_vector()) # only makes sense if vector valued
      if (length(private$genes) > 1) {
        paste0(private$fun_name, h_parens(h_short_list(private$genes)))
      } else {
        private$genes
      }
    },
    #' @description Extract the gene values from an assay as specified.
    #' @param assay (`matrix`)\cr original matrix with rownames containing the
    #'   specified genes.
    #' @return Either a vector with one value per column, or a matrix with multiple
    #'   genes in the rows.
    extract = function(assay) {
      assert_class(assay, "matrix")
      assert_names(rownames(assay), must.include = private$genes)
      assay_cols <- assay[private$genes, , drop = TRUE]
      if (length(private$genes) > 1 && is.function(private$fun)) {
        summary_res <- private$fun(assay_cols)
        assert_numeric(summary_res, len = ncol(assay_cols))
        summary_res
      } else {
        assay_cols
      }
    }
  ),
  private = list(
    genes = NULL,
    fun = NULL,
    fun_name = NULL
  )
)

#' `GeneSpec` Constructor
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Creates a new [`GeneSpec`] object.
#'
#' @param genes (`character` or `NULL`)\cr the gene IDs.
#' @param fun (`function` or `NULL`)\cr summary function. If `NULL` is
#'   used then multiple genes are not summarized but returned as a matrix from the
#'   `extract` method.
#' @param fun_name (`string`)\cr name of the summary function.
#'
#' @return A new [`GeneSpec`] object.
#'
#' @export
gene_spec <- function(genes,
                      fun = NULL,
                      fun_name = deparse(substitute(fun))) {
  GeneSpec$new(
    genes = genes,
    fun = fun,
    fun_name = fun_name
  )
}
