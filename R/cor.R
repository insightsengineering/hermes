#' Correlation between Samples of HermesData
#' 
#' This calculates the correlation matrix between the sample vectors of counts from 
#' a specified assay.
#' 
#' @param object (`AnyHermesData`)\cr object to calculate the correlation.
#' @param assay_name (`string`)\cr the assay name where the counts are located in.
#' @param method (`string`)\cr the correlation method, see [stats::cor()] for details.
#'   
#' @return A [HermesDataCor] object which is an extension of a [matrix] with
#'   additional quality flags in the slot `flag_data`: This contains the 
#'   `TechnicalFailureFlag` and `LowDepthFlag` columns describing the original 
#'   input samples.
#' 
#' @importFrom stats cor
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' calc_cor(object)
#' calc_cor(object, method = "spearman")
#'               
calc_cor <- function(object,
                     assay_name = "counts", 
                     method = "pearson") {
  assert_that(
    is_hermes_data(object),
    is.string(assay_name)
  )
  
  chosen_assay <- assay(object, assay_name)
  sample_cor_matrix <- stats::cor(chosen_assay, method = method)
  
  .HermesDataCor(
    sample_cor_matrix,
    flag_data = colData(object)[, c("TechnicalFailureFlag", "LowDepthFlag")]
  )
}

#' @rdname calc_cor  
#' @aliases HermesDataCor
#' @exportClass HermesDataCor 
.HermesDataCor <- setClass(
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)
