#' Control Settings for Counts Normalization
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This control function allows for easy customization of the normalization settings.
#'
#' @param log (`flag`)\cr whether `log2` values are returned, otherwise original scale is used.
#' @param lib_sizes (`NULL` or `counts`)\cr library sizes, if `NULL` the vector with the sum of the
#'   counts for each of the samples will be used.
#' @param prior_count (non-negative `number`)\cr average count to be added to each observation to
#'   avoid taking log of zero, used only when `log = TRUE`.
#'
#' @return List with the above settings used to perform the normalization procedure.
#'
#' @note To be used with the [normalize()] function.
#'
#' @export
#' @examples
#' control_normalize()
#' control_normalize(log = FALSE, lib_sizes = rep(1e6L, 20))
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

# normalize ----

#' Normalization of `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The `normalize()` method is normalizing the input [`AnyHermesData`] according to one or more
#' specified normalization methods. The results are saved as additional assays
#' in the object.
#'
#' Possible normalization methods (which are implemented with separate helper functions):
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
#'   a `prior_count` of 0.5 is combined with `lib_sizes` increased by 1 for each sample. Note that
#'   this is not required for the corresponding differential expression analysis, but just provided
#'   as a complementary experimental normalization approach here.
#'
#' @rdname normalize
#' @aliases normalize
#'
#' @param object (`AnyHermesData`)\cr object to normalize.
#' @param methods (`character`)\cr which normalization methods to use, see details.
#' @param control (named `list`)\cr settings produced by [control_normalize()].
#' @param ... not used.
#'
#' @return The [`AnyHermesData`] object with additional assays containing the normalized counts.
#'   The `control` is saved in the `metadata` of the object for future reference.
#'
#' @seealso [control_normalize()] to define the normalization method settings.
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
      fun_name <- paste0("hermes::h_", method)
      method_result <- eval(utils.nest::call_with_colon(
        fun_name,
        object = object,
        control = control
      ))
      assay(object, method) <- method_result
    }
    metadata(object) <- c(metadata(object), list(control_normalize = control))
    object
  }
)

#' @describeIn normalize calculates the Counts per Million (CPM) normalized counts.
#'
#' @export
#' @examples
#'
#' # Separate calculation of the CPM normalized counts.
#' counts_cpm <- h_cpm(a)
#' str(counts_cpm)
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

#' @describeIn normalize calculates the Reads per Kilobase per Million (RPKM) normalized counts.
#'
#' @export
#' @examples
#'
#' # Separate calculation of the RPKM normalized counts.
#' counts_rpkm <- h_rpkm(a)
#' str(counts_rpkm)
h_rpkm <- function(object,
                   control = control_normalize()) {
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

#' @describeIn normalize calculates the Transcripts per Million (TPM) normalized counts.
#'
#' @export
#' @examples
#'
#' # Separate calculation of the TPM normalized counts.
#' counts_tpm <- h_tpm(a)
#' str(counts_tpm)
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

#' @describeIn normalize calculates the VOOM normalized counts. `r lifecycle::badge("experimental")`
#'
#' @export
#' @examples
#'
#' # Separate calculation of the VOOM normalized counts.
#' counts_voom <- h_voom(a)
#' str(counts_voom)
h_voom <- function(object,
                   control = control_normalize()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, c("lib_sizes", "log"))
  )
  cnts <- counts(object)
  if (S4Vectors::isEmpty(cnts)) {
    return(cnts)
  }
  norm_log2 <- limma::voom(
    counts = cnts,
    lib.size = control$lib_sizes
  )$E
  if (control$log) {
    norm_log2
  } else {
    2^norm_log2
  }
}
