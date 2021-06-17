#' `limma`/`voom` Differential Expression Analysis
#'
#' This helper functions performs the differential expression analysis with the `voom`
#' method from the `limma` package (via [limma::voom()], [limma::lmFit()] and [limma::eBayes()])
#' for given counts in a [AnyHermesData] object and a corresponding `design` matrix.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix.
#' @return A data frame with columns `log2_fc` (estimated log2 fold change),
#'   `stat` (moderated t-statistic), `p_val` (raw p-value), `adj_p_pval` (Benjamini-Hochberg
#'   adjusted p-value).
#'
#' @importFrom limma voom lmFit eBayes topTable
#' @export
#'
#' @references
#' \insertRef{limma_package}{hermes}
#'
#' \insertRef{voom_method}{hermes}
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' design <- model.matrix(~SEX, colData(object))
#' result <- h_diff_expr_voom(object, design)
#' head(result)
h_diff_expr_voom <- function(object, design) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design),
    identical(dim(design), c(ncol(object), 2L))
  )
  voom_counts <- limma::voom(counts(object))
  lm_fit <- limma::lmFit(voom_counts, design)
  eb_stats <- limma::eBayes(lm_fit)
  top_tab <- limma::topTable(
    eb_stats,
    coef = 2L,
    number = Inf, # Retain all genes.
    adjust.method = "BH",
    sort.by = "p" # Sort by p-value.
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

#' DESeq2 Differential Expression Analysis
#'
#' This helper functions performs the differential expression analysis with
#' [DESeq2::DESeq()] for a given [AnyHermesData] input and `design` matrix.
#'
#' @param object (`HermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix.
#' @return A data frame with columns `log2_fc` (estimated log2 fold change),
#'   `stat` (Wald statistic), `p_val` (raw p-value), `adj_p_pval` (Benjamini-Hochberg adjusted p-value).
#'
#' @importFrom DESeq2 DESeqDataSet DESeq results
#' @export
#'
#' @references
#' \insertRef{DESeq2_package}{hermes}
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' design <- model.matrix(~SEX, colData(object))
#' result <- h_diff_expr_deseq2(object, design)
#' head(result)
h_diff_expr_deseq2 <- function(object, design) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design),
    identical(dim(design), c(ncol(object), 2L))
  )
  deseq_data <- DESeq2::DESeqDataSet(se = object, design = design)
  deseq_data_processed <- DESeq2::DESeq(deseq_data, quiet = TRUE)
  deseq_data_res <- DESeq2::results(deseq_data_processed)
  deseq_data_res_df <- as.data.frame(deseq_data_res)
  adj_pval_order <- order(deseq_data_res_df$padj)
  deseq_data_res_df_sorted <- deseq_data_res_df[adj_pval_order, ]
  with(
    deseq_data_res_df_sorted,
    data.frame(
      log2_fc = log2FoldChange,
      stat = stat,
      p_val = pvalue,
      adj_p_val = padj,
      row.names = rownames(deseq_data_res_df_sorted)
    )
  )
}

#' Differential Expression Analysis
#'
#' This function performs differential expression analysis
#' using a method of preference.
#'
#' Possible method choices are:
#' - `voom`: uses [limma::voom()], see [h_diff_expr_voom()] for details.
#' - `deseq2`: uses [DESeq2::DESeq()], see [h_diff_expr_deseq2()] for details.
#'
#' @param object (`AnyHermesData`)\cr input. Note that this function only uses the
#'   original counts for analysis, so this does not need to be normalized.
#' @param group (`string`)\cr name of factor variable with 2 levels in `colData(object)`.
#'   These 2 levels will be compared in the differential expression analysis.
#' @param method (`string`)\cr method for differential expression analysis, see details below.
#'
#' @return A [`HermesDataDiffExpr`] object which is a data frame with the following columns for each gene
#'   in the [`HermesData`] object:
#'   - `log2_fc` (estimate of the log2 fold change between the 2 levels of the
#'   provided factor)
#'   - `stat` (the test statistic, which one depends on the method used)
#'   - `p_val` (the raw p-value),
#'   - `adj_p_val` (the adjusted p-value) values from differential expression analysis for each feature / gene .
#'
#' @importFrom stats as.formula model.matrix
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment) %>%
#'   add_quality_flags() %>%
#'   filter()
#' colData(object)$SEX <- factor(colData(object)$SEX) # nolint
#' res1 <- diff_expression(object, group = "SEX", method = "voom")
#' head(res1)
#' res2 <- diff_expression(object, group = "SEX", method = "deseq2")
#' head(res2)
diff_expression <- function(object,
                            group,
                            method = c("voom", "deseq2")) {
  assert_that(
    is_hermes_data(object),
    is.string(group),
    tern::is_df_with_nlevels_factor(
      df = as.data.frame(colData(object)),
      variable = group,
      n_levels = 2L
    )
  )
  method <- match.arg(method, c("voom", "deseq2"))

  if (anyNA(get_tech_failure(object))) {
    warning("NAs in technical failure flags, please make sure to use `add_quality_flags()` beforehand")
  }
  if (anyNA(get_low_depth(object))) {
    warning("NAs in low depth flags, please make sure to use `add_quality_flags()` beforehand")
  }
  if (anyNA(get_low_expression(object))) {
    warning("NAs in low expression flags, please make sure to use `add_quality_flags()` beforehand")
  }
  form <- stats::as.formula(paste("~", group))
  design <- stats::model.matrix(form, data = colData(object))
  result <- switch(
    method,
    "voom" = h_diff_expr_voom(object, design),
    "deseq2" = h_diff_expr_deseq2(object, design)
  )
  .HermesDataDiffExpr(result)
}

# HermesDataDiffExpr ----

.diff_expr_cols <- c(
  "log2_fc",
  "stat",
  "p_val",
  "adj_p_val"
)

setOldClass("data.frame")

#' @rdname diff_expression
#' @aliases HermesDataDiffExpr
#' @exportClass HermesDataDiffExpr
.HermesDataDiffExpr <- setClass(
  Class = "HermesDataDiffExpr",
  contains = "data.frame"
)

S4Vectors::setValidity2(
  Class = "HermesDataDiffExpr",
  method = function(object) {
    msg <- validate_cols(
      required = .diff_expr_cols,
      actual = colnames(object)
    )
    if (is.null(msg)) TRUE else msg
  }
)
