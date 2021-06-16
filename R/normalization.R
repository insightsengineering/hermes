#' Control Settings for Counts Normalization
#'
#' @param log (`flag`)\cr whether `log2` values are returned, otherwise original scale is used.
#' @param lib_sizes (`numeric`)\cr library sizes, the default is the vector with the sum of the
#'   counts for each of the samples.
#' @param prior_count (`count`)\cr average count to be added to each observation to avoid
#'   taking log of zero, used only when `log = TRUE`.
#'
#' @return List with the above settings used to perform the normalization procedure.
#'
#' @note To be used with the `normalize()` function.
#'
#' @export
#' @examples
#' control_normalize()
#' control_normalize(log = FALSE, lib_sizes = rep(1e6L, 20))
#'
control_normalize <- function(log = TRUE,
                              lib_sizes = NULL,
                              prior_count = 1) {
  assert_that(
    is.flag(log),
    is.null(lib_sizes) || is_counts_vector(lib_sizes),
    is.number(prior_count) && prior_count >= 0
  )
  list(
    log = log,
    lib_sizes = lib_sizes,
    prior_count = prior_count
  )
}

#' Counts per Million (CPM) Normalization
#'
#' @param object (`HermesData`) \cr input.
#' @param control (`list`) \cr list of settings used to perform the normalization procedure.
#' @return A numeric matrix with normalized counts using the CPM method.
#'
#' @export
#' @importFrom edgeR cpm
#' @examples
#' h <- HermesData(summarized_experiment)
#' cont <- control_normalize()
#' counts_cpm <- h_cpm(h, cont)
#' str(counts_cpm)
#'
h_cpm <- function(object,
                  control = control_normalize()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, c("lib_sizes", "log", "prior_count"))
  )
  edgeR::cpm(
    y = counts(object),
    lib.size = control$lib_sizes,
    log = control$log,
    prior.count = control$prior_count
  )
}

#' Reads per Kilobase per Million (RPKM) Normalization
#'
#' @param object (`AnyHermesData`)\cr input object.
#' @param control (`list`)\cr list of settings used to perform the normalization procedure.
#' @return A numeric matrix with normalized counts using the RPKM method.
#'
#' @note To be used with the `normalize()` function.
#'
#' @export
#' @importFrom edgeR rpkm
#' @examples
#' h <- HermesData(summarized_experiment)
#' cont <- control_normalize(log = FALSE, lib_sizes = rep(1e6L, 20))
#' counts_rpkm <- h_rpkm(h, cont)
#' str(counts_rpkm)
#'
h_rpkm <- function(object,
                   control) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, c("lib_sizes", "log", "prior_count")),
    noNA(rowData(object)$WidthBP)
  )
  edgeR::rpkm(
    y = counts(object),
    gene.length = rowData(object)$WidthBP,
    lib.size = control$lib_sizes,
    log = control$log,
    prior.count = control$prior_count
  )
}

#' Transcripts per Million (TPM) Normalization
#'
#' @param object (`HermesData`)\cr input.
#' @param control (`list`)\cr list of settings used to perform the normalization procedure.
#' @return A numeric matrix with normalized counts using the TPM method.
#'
#' @export
#' @examples
#' h <- HermesData(summarized_experiment)
#' cont <- control_normalize()
#' counts_tpm <- h_tpm(h, cont)
#' str(counts_tpm)
#'
h_tpm <- function(object,
                  control = control_normalize()) {
  rpkm <- h_rpkm(object, control_normalize(log = FALSE))
  rpkm_sums <- colSums(rpkm, na.rm = TRUE)
  tpm <- sweep(rpkm, MARGIN = 2, STATS = rpkm_sums, FUN = "/") * 1e6
  if (control$log) {
    log2(tpm + control$prior_count)
  } else {
    tpm
  }
}

#' VOOM Normalization
#'
#' @param object (`HermesData`)\cr input.
#' @param control (`list`)\cr list of settings used to perform the normalization procedure.
#'
#' @return A numeric matrix with normalized counts using the VOOM method.
#'
#' @export
#' @importFrom limma voom
#' @examples
#' h <- HermesData(summarized_experiment)
#' cont <- control_normalize()
#' counts_voom <- h_voom(h, cont)
#' str(counts_voom)
#'
h_voom <- function(object,
                   control = control_normalize()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, c("lib_sizes", "log"))
  )
  norm_log2 <- limma::voom(
    counts = counts(object),
    lib.size = control$lib_sizes
  )$E
  if (control$log) {
    norm_log2
  } else {
    2^norm_log2
  }
}

#' Normalization of HermesData
#'
#' This method is normalizing the input [HermesData] according to one or more
#' specified normalization methods. The results are saved as assitional assays
#' in the object.
#'
#' Possible normalization methods are:
#' - `cpm`: Counts per Million (CPM). Separately by sample, the original counts of the genes
#'   are divided by the library size of this sample, and multiplied by one million. This is the
#'   appropriate normalization for between-sample comparisons.
#' - `rpkm`: Reads per Kilobase of transcript per Million reads mapped (RPKM). Each gene count is
#'   divided by the gene width (in kilobases) and then again divided by the library sizes of each
#'   sample (in millions). This allows for within-sample comparisons, as it takes
#'   into account the gene lengths - longer genes will always have more counts than shorter genes.
#' - `tpm`: Transcripts per Million (TPM). This addresses the problem of RPKM being inconsistent
#'   across samples (which can be seen that the sum of all RPKM values will vary from sample to
#'   sample). Therefore here we divide the RPKM by the sum of all RPKM values for each sample,
#'   and multiply by one million.
#' - `voom`: VOOM normalization. This is essentially just a slight variation of CPM where
#'   a `prior_count` of 0.5 is combined with `lib_sizes` increased by 1 for each sample. It is used
#'   as a starting point for the corresponding differential expression analysis.
#'
#' @rdname normalize
#' @aliases normalize
#'
#' @param object (`AnyHermesData`)\cr object to normalize.
#' @param methods (`character`)\cr which normalization methods to use, see details.
#' @param control (named `list`)\cr settings produced by [control_normalize()].
#' @param ... not used.
#'
#' @return The [AnyHermesData] object with additional assays containing the normalized counts.
#'   The `control` is saved in the `metadata` of the object for future reference.
#'
#' @importFrom BiocGenerics normalize
#' @export
#' @examples
#' a <- HermesData(summarized_experiment)
#'
#' # By default, log values are used with a prior count of 1 added to original counts.
#' result <- normalize(a)
#' assayNames(result)
#' tpm <- assay(result, "tpm")
#' tpm[1:3, 1:3]
#'
#' # We can also work on original scale.
#' result_orig <- normalize(a, control = control_normalize(log = FALSE))
#' tpm_orig <- assay(result_orig, "tpm")
#' tpm_orig[1:3, 1:3]
#'
setMethod(
  f = "normalize",
  signature = "AnyHermesData",
  definition = function(object,
                        methods = c("cpm", "rpkm", "tpm", "voom"),
                        control = control_normalize(),
                        ...) {
    method_choices <- c("cpm", "rpkm", "tpm", "voom")
    assert_that(all(methods %in% method_choices))
    methods <- match.arg(methods, choices = method_choices, several.ok = TRUE)
    for (method in methods) {
      fun_name <- paste0("h_", method)
      assay(object, method) <- do.call(
        fun_name,
        list(object = object, control = control),
        envir = as.environment("package:hermes")
      )
    }
    metadata(object) <- c(metadata(object), list(control_normalize = control))
    object
  }
)
