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
#' h <- HermesData(summarized_experiment)
#' cont <- control_normalize()
#' counts_voom <- h_voom(h, cont)
#'               
h_voom <- function(object, 
                   control) {
  assert_that(
    is_hermes_data(object),
    utils.nest::is_fully_named_list(control)
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
