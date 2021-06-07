# HermesDataCor ----

#' HermesData Correlation
#' 
#' The [HermesDataCor] class is an extension of a [matrix] with additional quality flags.
#' 
#' @note The `cor()` function will return an object of [HermesDataCor] class. 
#' @aliases HermesDataCor
#' @exportClass HermesDataCor
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' cor(x = object)
#' # which will return an object of HermesDataCor. 
#'          
.HermesDataCor <- setClass(
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)

# cor ----

setGeneric("cor")

#' Correlation Function
#' 
#' @rdname cor
#' @aliases cor
#' 
#' @param x (`AnyHermesData`)\cr object to calculate the correlation.
#' @param y (`string`)\cr the assay name where the counts are located in x (AnyHermesData object).
#' @param use not used.
#' @param method (`string`)\cr the correlation coefficient (or covariance) to be computed, either "pearson", "kendall", or "spearman". 
#'   
#' @return A [HermesDataCor] object with calculated correlations and QC flags (technical failure and low depth).
#' 
#' @importFrom stats cor
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' cor(x = object)
#' cor(x = object, method = "spearman")
#'               
setMethod(
  f = "cor",
  signature = signature(x = "AnyHermesData"),
  definition = function(x, y = "counts", use = NULL, method = "pearson") {
    assert_that(
      is.string(y),
      is.null(use)
    )
    
    chosen_assay <- assay(x, y)
    sample_cor_matrix <- stats::cor(chosen_assay, method = method)
    
    .HermesDataCor(
      sample_cor_matrix,
      flag_data = colData(x)[, c("TechnicalFailureFlag", "LowDepthFlag")]
    )
  }
)
