#' `limma`/voom Differential Expression Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper functions performs the differential expression analysis with the voom
#' method from the `limma` package (via [limma::voom()], [limma::lmFit()] and [limma::eBayes()])
#' for given counts in a [AnyHermesData] object and a corresponding `design` matrix.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix.
#' @param ... additional arguments internally passed to [limma::eBayes()] (`robust`, `trend`, `proportion`,
#'   `winsor.tail.p`, `stdev.coef.lim`).
#' @return A data frame with columns `log2_fc` (estimated log2 fold change),
#'   `stat` (moderated t-statistic), `p_val` (raw p-value), `adj_p_pval` (Benjamini-Hochberg adjusted p-value).
#'
#' @export
#'
#' @references
#' \insertRef{limma_package}{hermes}
#'
#' \insertRef{voom_method}{hermes}
#'
#' @examples
#' object <- hermes_data
#'
#' # Create the design matrix corresponding to the factor of interest.
#' design <- model.matrix(~SEX, colData(object))
#'
#' # Then perform the differential expression analysis.
#' result <- h_diff_expr_voom(object, design)
#' head(result)
#'
#' # Sometimes we might want to specify method details.
#' result2 <- h_diff_expr_voom(object, design, trend = TRUE, robust = TRUE)
#' head(result2)
h_diff_expr_voom <- function(object, design, ...) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design),
    identical(dim(design), c(ncol(object), 2L))
  )
  voom_counts <- limma::voom(counts(object))
  lm_fit <- limma::lmFit(voom_counts, design)
  eb_stats <- limma::eBayes(lm_fit, ...)
  top_tab <- limma::topTable(
    eb_stats,
    coef = 2L,
    number = Inf, # Retain all genes.
    sort.by = "p" # Sort by p-value.
  )
  data.frame(
    log2_fc = top_tab$logFC,
    stat = top_tab$t,
    p_val = top_tab$P.Value,
    adj_p_val = top_tab$adj.P.Val,
    row.names = rownames(top_tab)
  )
}

#' `DESeq2` Differential Expression Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper functions performs the differential expression analysis with
#' [DESeq2::DESeq()] for a given [AnyHermesData] input and `design` matrix.
#'
#' @param object (`HermesData`)\cr input.
#' @param design (`matrix`)\cr design matrix.
#' @param ... additional arguments internally passed to [DESeq2::DESeq()] (`fitType`, `sfType`,
#'   `minReplicatesForReplace`, `useT`, `minmu`).
#' @return A data frame with columns `log2_fc` (estimated log2 fold change),
#'   `stat` (Wald statistic), `p_val` (raw p-value), `adj_p_pval` (Benjamini-Hochberg adjusted p-value).
#'
#' @export
#'
#' @references
#' \insertRef{DESeq2_package}{hermes}
#'
#' @examples
#' object <- hermes_data
#'
#' # Create the design matrix corresponding to the factor of interest.
#' design <- model.matrix(~SEX, colData(object))
#'
#' # Then perform the `DESeq2` differential expression analysis.
#' result <- h_diff_expr_deseq2(object, design)
#' head(result)
#'
#' # Change of the `fitType` can be required in some cases.
#' result2 <- h_diff_expr_deseq2(object, design, fitType = "local")
#' head(result2)
h_diff_expr_deseq2 <- function(object, design, ...) {
  assert_that(
    is_hermes_data(object),
    is.matrix(design),
    identical(dim(design), c(ncol(object), 2L))
  )
  deseq_data <- DESeq2::DESeqDataSet(se = object, design = design)
  deseq_data_processed <- DESeq2::DESeq(
    deseq_data,
    quiet = TRUE,
    test = "Wald",
    betaPrior = FALSE,
    parallel = FALSE,
    modelMatrixType = "standard",
    ...
  )
  deseq_data_res <- DESeq2::results(deseq_data_processed)
  deseq_data_res_df <- as.data.frame(deseq_data_res)
  adj_pval_order <- order(deseq_data_res_df$padj)
  deseq_data_res_df_sorted <- deseq_data_res_df[adj_pval_order, ]
  data.frame(
    log2_fc = deseq_data_res_df_sorted$log2FoldChange,
    stat = deseq_data_res_df_sorted$stat,
    p_val = deseq_data_res_df_sorted$pvalue,
    adj_p_val = deseq_data_res_df_sorted$padj,
    row.names = rownames(deseq_data_res_df_sorted)
  )
}

#' Differential Expression Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `diff_expression()` function performs differential expression analysis
#' using a method of preference.
#'
#' A corresponding `autoplot()` method is visualizing the results as a volcano plot.
#'
#' @details Possible method choices are:
#' - voom: uses [limma::voom()], see [h_diff_expr_voom()] for details.
#' - `deseq2`: uses [DESeq2::DESeq()], see [h_diff_expr_deseq2()] for details.
#'
#' @param object (`AnyHermesData`)\cr input. Note that this function only uses the
#'   original counts for analysis, so this does not need to be normalized.
#' @param group (`string`)\cr name of factor variable with 2 levels in `colData(object)`.
#'   These 2 levels will be compared in the differential expression analysis.
#' @param method (`string`)\cr method for differential expression analysis, see details below.
#' @param ... additional arguments passed to the helper function associated with the selected method.
#'
#' @return A [`HermesDataDiffExpr`] object which is a data frame with the following columns for each gene
#'   in the [`HermesData`] object:
#'   - `log2_fc` (the estimate of the log2 fold change between the 2 levels of the
#'      provided factor)
#'   - `stat` (the test statistic, which one depends on the method used)
#'   - `p_val` (the raw p-value)
#'   - `adj_p_val` (the multiplicity adjusted p-value value)
#'
#' @note
#'   - We provide the [df_cols_to_factor()] utility function that makes it easy to convert the
#'     `colData()` character and logical variables to factors, so that they can be subsequently
#'     used as `group` inputs. See the example.
#'   - In order to avoid a warning when using `deseq2`, it can be necessary to specify
#'     `fitType = "local"` as additional argument. This could e.g. be the case when only few samples
#'     are present in which case the default parametric dispersions estimation will not work.
#'
#' @export
#'
#' @examples
#' object <- hermes_data %>%
#'   add_quality_flags() %>%
#'   filter()
#'
#' # Convert character and logical to factor variables in `colData`,
#' # including the below used `group` variable.
#' colData(object) <- df_cols_to_factor(colData(object))
#' res1 <- diff_expression(object, group = "SEX", method = "voom")
#' head(res1)
#' res2 <- diff_expression(object, group = "SEX", method = "deseq2")
#' head(res2)
#'
#' # Pass method arguments to the internally used helper functions.
#' res3 <- diff_expression(object, group = "SEX", method = "voom", robust = TRUE, trend = TRUE)
#' head(res3)
#' res4 <- diff_expression(object, group = "SEX", method = "deseq2", fitType = "local")
#' head(res4)
diff_expression <- function(object,
                            group,
                            method = c("voom", "deseq2"),
                            ...) {
  assert_that(
    is_hermes_data(object),
    is.string(group)
  )
  expect_factor(colData(object)[[group]], n.levels = 2L)

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
  result <- switch(method,
    "voom" = h_diff_expr_voom(object, design, ...),
    "deseq2" = h_diff_expr_deseq2(object, design, ...)
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
.HermesDataDiffExpr <- setClass( # nolint
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

# autoplot(HermesDataDiffExpr) ----

#' @describeIn diff_expression generates a volcano plot for a [`HermesDataDiffExpr`] object.
#'
#' @param adj_p_val_thresh (`proportion`)\cr threshold on the adjusted p-values (y-axis) to
#'   flag significance.
#' @param log2_fc_thresh (`number`)\cr threshold on the absolute log2 fold-change (x-axis)
#'   to flag up- or down-regulation of transcription.
#'
#' @export
#'
#' @examples
#'
#' # Create the corresponding volcano plots.
#' autoplot(res1)
#' autoplot(res3)
setMethod(
  f = "autoplot",
  signature = signature(object = "HermesDataDiffExpr"),
  definition = function(object,
                        adj_p_val_thresh = 0.05,
                        log2_fc_thresh = 2.5) {
    expect_proportion(adj_p_val_thresh)
    assert_that(
      is.number(log2_fc_thresh),
      log2_fc_thresh > 0
    )

    df <- as.data.frame(object)
    df$low_p_val <- df$adj_p_val < adj_p_val_thresh
    df$high_log2_fc <- abs(df$log2_fc) >= log2_fc_thresh
    df$flag_gene <- df$low_p_val & df$high_log2_fc
    df$diff_expr <- ifelse(
      !df$flag_gene,
      "NO",
      ifelse(
        df$log2_fc < 0,
        "DOWN",
        "UP"
      )
    )
    df$label <- ifelse(df$flag_gene, rownames(df), NA)

    ggplot(
      data = df,
      aes(
        x = .data$log2_fc,
        y = -log10(.data$adj_p_val),
        color = .data$diff_expr,
        label = .data$label
      )
    ) +
      geom_point() +
      ggrepel::geom_text_repel(na.rm = TRUE, show.legend = FALSE, max.overlaps = 100) +
      xlab("log2 fold change") +
      ylab("-log10 adjusted p-value") +
      labs(color = "Difference") +
      geom_vline(xintercept = c(-1, 1) * log2_fc_thresh, col = "black") +
      geom_hline(yintercept = -log10(adj_p_val_thresh), col = "black")
  }
)
