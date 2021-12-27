#' Creation of Unique Labels
#'
#' This helper function generates a set of unique labels given
#' unique IDs and not necessarily unique names.
#'
#' @param ids (`character` or `NULL`)\cr unique IDs.
#' @param nms (`character` or `NULL`)\cr not necessarily unique names if provided.
#'
#' @return Character vector where empty names are replaced by the IDs and
#'   non-unique names are made unique by appending the IDs in parentheses.
#' @export
#'
#' @examples
#' h_unique_labels(c("1", "2", "3"), c("A", "B", "A"))
#' h_unique_labels(NULL)
#' h_unique_labels(c("1", "2", "3"))
h_unique_labels <- function(ids, nms = NULL) {
  if (is.null(ids)) {
    return(NULL)
  }
  assert_character(ids, any.missing = FALSE, unique = TRUE)
  if (is.null(nms)) {
    return(ids)
  }
  assert_character(nms, any.missing = FALSE)
  assert_true(identical(length(ids), length(nms)))

  res <- ifelse(nms == "", ids, nms)
  are_duplicate <- h_all_duplicated(res)
  if (any(are_duplicate)) {
    res[are_duplicate] <- paste(
      res[are_duplicate],
      h_parens(ids[are_duplicate])
    )
  }
  assert_character(res, any.missing = FALSE, unique = TRUE)
  res
}

#' R6 Class Representing a Gene (Signature) Specification
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A `GeneSpec` consists of the gene IDs (possibly named with labels),
#' the summary function and the name of the summary function.
#'
#' @export
#'
#' @examples
#' # Minimal specification if only one gene is used.
#' x_spec <- gene_spec("GeneID:1820")
#'
#' # Using multiple genes with a signature.
#' x_spec <- gene_spec(c("GeneID:1820", "GeneID:52"), fun = colMeans)
#' x_spec <- gene_spec(c("GeneID:1820", "GeneID:52"), fun = colPrinComp1)
#' x_spec$returns_vector()
#' x_spec$get_genes()
#' x_spec$get_gene_labels()
#' x_spec$get_label()
#'
#' # Using multiple genes with partial labels, without a signature.
#' x_spec <- gene_spec(c(A = "GeneID:1820", "GeneID:52"))
#' x_spec$returns_vector()
#' x_spec$get_gene_labels()
#'
#' # Use the gene specification to extract genes from a matrix.
#' mat <- matrix(
#'   data = rpois(15, 10),
#'   nrow = 3, ncol = 5,
#'   dimnames = list(c("GeneID:1820", "GeneID:52", "GeneID:523"), NULL)
#' )
#' x_spec$extract(mat)
#'
#' # We can also extract these as a `data.frame`.
#' x_spec$extract_data_frame(mat)
GeneSpec <- R6::R6Class(
  "GeneSpec",
  public = list(
    #' @description Creates a new [`GeneSpec`] object.
    #' @param genes (named `character` or `NULL`)\cr the gene IDs, where the names
    #'   are used as labels if available.
    #' @param fun (`function` or `NULL`)\cr summary function. If `NULL` is
    #'   used then multiple genes are not summarized but returned as a matrix from the
    #'   `extract` method.
    #' @param fun_name (`string`)\cr name of the summary function.
    #' @return A new [`GeneSpec`] object.
    initialize = function(genes = NULL,
                          fun = NULL,
                          fun_name = deparse(substitute(fun))) {
      assert_character(genes, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
      assert_function(fun, null.ok = TRUE)
      assert_string(fun_name, min.chars = 1L)

      private$gene_labels <- h_unique_labels(ids = genes, nms = names(genes))
      private$genes <- genes
      private$fun <- fun
      private$fun_name <- fun_name
    },
    #' @description Returns the genes.
    get_genes = function() {
      private$genes
    },
    #' @description Returns the gene labels (substituted by gene IDs if not available).
    #' @param genes (`character`)\cr for which subset of genes the labels should be returned.
    get_gene_labels = function(genes = self$get_genes()) {
      index <- match(genes, private$genes)
      assert_integer(index, any.missing = FALSE)
      private$gene_labels[index]
    },
    #' @description Predicate whether the extract returns a vector or not.
    returns_vector = function() {
      (length(private$genes) == 1) || is.function(private$fun)
    },
    #' @description Returns a string which can be used e.g. for plot labels.
    #' @param genes (`character`)\cr for which subset of genes the labels should be returned.
    get_label = function(genes = self$get_genes()) {
      gene_labels <- self$get_gene_labels(genes)
      if (length(private$genes) > 1) {
        gene_list <- h_parens(h_short_list(gene_labels))
        if (self$returns_vector()) {
          paste0(private$fun_name, gene_list)
        } else {
          gene_list
        }
      } else {
        private$gene_labels
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
      assay_cols <- assay[private$genes, , drop = FALSE]
      if (self$returns_vector()) {
        res <- if (is.function(private$fun)) {
          private$fun(assay_cols)
        } else {
          as.vector(assay_cols)
        }
        assert_numeric(res, len = ncol(assay_cols))
        stats::setNames(
          res,
          colnames(assay)
        )
      } else {
        rownames(assay_cols) <- private$gene_labels
        assay_cols
      }
    },
    #' @description Extract the gene values as a `data.frame`.
    #' @param assay (`matrix`)\cr original matrix with rownames containing the
    #'   specified genes.
    #' @return A `data.frame` with the genes in the columns and the samples
    #'   in the rows.
    extract_data_frame = function(assay) {
      gene_matrix <- self$extract(assay)
      if (!is.matrix(gene_matrix)) {
        gene_matrix <- t(gene_matrix)
      }
      num_genes <- nrow(gene_matrix)
      gene_names <- if (num_genes == 1) {
        self$get_label()
      } else {
        self$get_gene_labels()
      }
      gene_names <- make.names(gene_names, unique = TRUE)
      rownames(gene_matrix) <- gene_names
      data.frame(t(gene_matrix))
    }
  ),
  private = list(
    genes = NULL,
    gene_labels = NULL,
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
#' @param genes (named `character` or `NULL`)\cr the gene IDs, where the names
#'   are used as labels if available.
#' @param fun (`function` or `NULL`)\cr summary function. If `NULL` is
#'   used then multiple genes are not summarized but returned as a matrix from the
#'   `extract` method.
#' @param fun_name (`string`)\cr name of the summary function.
#'
#' @return A new [`GeneSpec`] object.
#'
#' @export
#'
#' @examples
#' gene_spec("GeneID:11185")
#' gene_spec(c("GeneID:11185", "GeneID:10677", "GeneID:101928428"), fun = colMeans)
gene_spec <- function(genes = NULL,
                      fun = NULL,
                      fun_name = deparse(substitute(fun))) {
  GeneSpec$new(
    genes = genes,
    fun = fun,
    fun_name = fun_name
  )
}
