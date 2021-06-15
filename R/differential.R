#' Functions for Differential Gene Expression Analysis
#'
#' This creates a histogram of the library sizes of the [HermesDataDiffExpr] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param group (`string`)\cr color of the bars filling.
#' @param method (`string`)\cr we support limma-voom (limma_voom) and DESeq/DESeq2 (deseq2) tools.
#' @return A `HermesDataDiffExpr` object.
#' 
#' @importFrom rlang .data 
#' @export
#' @examples
#' object <- HermesData(summarized_experiment) %>% add_quality_flags() %>% filter()
#' diff_expression(object, "SEX", method = "limma_voom")
#' diff_expression(object, "SEX", method = "deseq2")
#'
diff_expression <- function(object,
                            group,
                            method = c("limma_voom", "deseq2")) {
  assert_that(
    is_hermes_data(object),
    is.string(group)
    #tern::is_df_with_nlevels_factor(
    #  df = as.data.frame(colData(object)), 
    #  variable = group, 
    #  n_levels = 2L
    #)
  )
  method <- match.arg(method, c("limma_voom", "deseq2"))
  
  if (anyNA(get_tech_failure(object))) {
    warning("NAs in technical failure flags, please make sure to use `add_quality_flags()` beforehand")
  }
  if (anyNA(get_low_depth(object))) {
    warning("NAs in low depth flags, please make sure to use `add_quality_flags()` beforehand")
  }
  if (anyNA(get_low_expression(object))) {
    warning("NAs in low expression flags, please make sure to use `add_quality_flags()` beforehand")
  }
  form <- as.formula(paste("~", group))
  design <- model.matrix(form, data = colData(object))  
  result <- switch(
    method,
    "limma_voom" = h_diff_expr_limma(object, design),
    "deseq2" = h_diff_expr_deseq2(object, design)
  )
  invisible(.HermesDataDiffExpr(result))
}



#' @rdname diff_expression
#' @exportClass HermesDataDiffExpr
#' @importFrom S4Vectors setValidity2
.HermesDataDiffExpr <- setClass(
  Class = "HermesDataDiffExpr",
  contains = "data.frame"
)


# HermesDataDiffExpr validity 

S4Vectors::setValidity2(
  Class = "HermesDataDiffExpr",
  method = function(object) {
    .diff_expr_cols <- c(
      "log2_fc",
      "stat",
      "p_val",
      "adj_p_val"
    )
    msg <- validate_cols(
      required = .diff_expr_cols,
      actual = colnames(object)
    )
    if (is.null(msg)) TRUE else msg
  }
)

#' Helper function to use limma-voom method
#'
#' This creates a data frame with folder change and p-values for all genes  
#'
#' @param object (`HermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix to run limma-voom analysis.
#' @return A data frame with limma-voom analysis results.
#' 
#' @importFrom rlang .data
#' @importFrom limma voom
#' @importFrom limma lmFit
#' @importFrom limma eBayes
#' @importFrom limma topTable
#' @export
#'
h_diff_expr_limma <- function(object, design) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design)
  )
  obj_count_voom <- limma::voom(counts(object))
  fit <- limma::lmFit(obj_count_voom, design)
  eb <- limma::eBayes(fit)
  top_tab <- limma::topTable(
    eb, 
    coef = 2L, 
    n = nrow(obj_count_voom),  # Retain all genes.
    sort.by = "p"  # Use adjusted p-value to sort.
  ) 
  with(
    top_tab,
    data.frame(
      log2_fc = logFC,
      stat = t,
      p_val = P.Value,
      adj_p_val = adj.P.Val,
      row.names = rownames(top_tab)
    )
  )
}

#' Helper function to use DESeq2 method
#'
#' This creates a data frame with folder change and p-values for all genes  
#'
#' @param object (`HermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix to run DESeq2 analysis.
#' @return A data frame with DESeq2 analysis results.
#' 
#' @importFrom rlang .data
#' @export
h_diff_expr_deseq2 <- function(object, design) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design)
  )
  deseq_data <- DESeqDataSet(se = object, design = design)
  deseq_data_processed <- DESeq(deseq_data, quiet = TRUE)
  deseq_data_res <- results(deseq_data_processed) # Note: this has multiple options we might want to use.
  deseq_data_res_df <- as.data.frame(deseq_data_res)
  deseq_data_res_df <- deseq_data_res_df[order(deseq_data_res_df$padj), ]  # Use adj p-value to sort.
  with(
    deseq_data_res_df,
    data.frame(
      log2_fc = log2FoldChange,
      stat = stat,
      p_val = pvalue,
      adj_p_val = padj,
      row.names = rownames(deseq_data_res_df)
    )
  )
}

 