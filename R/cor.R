#' @rdname calc_cor
#' @aliases calc_cor  
#' @aliases HermesDataCor
#' @exportClass HermesDataCor 
#' 
.HermesDataCor <- setClass(
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)

setGeneric("calc_cor",
           function(object, assay_name = "counts", method = "pearson")
           {standardGeneric("calc_cor")}
)

#' Correlation between Samples of HermesData
#' 
#' @describeIn calc_cor This calculates the correlation matrix between the sample vectors of counts from 
#' a specified assay, as a [HermesDataCor] object which is an extension of a [matrix] with
#' additional quality flags in the slot `flag_data`: This contains the 
#' `TechnicalFailureFlag` and `LowDepthFlag` columns describing the original input samples.
#' 
#' @param object (`AnyHermesData`)\cr object to calculate the correlation.
#' @param assay_name (`string`)\cr the assay name where the counts are located in.
#' @param method (`string`)\cr the correlation method, see [stats::cor()] for details.
#'   
#' @return A [HermesDataCor] object.
#' 
#' @importFrom stats cor
#' 
#' @export
#' 
#' @examples
#' object <- HermesData(summarized_experiment)
#' calc_cor(object)
#' result <- calc_cor(object, method = "pearson")
#'               
setMethod(
  f = "calc_cor",
  signature = "AnyHermesData",
  definition = function(object,
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
)

#' @describeIn calc_cor This plot method uses the [ComplexHeatmap::Heatmap()] function
#'   to plot the correlations between samples saved in a [HermesDataCor] object.
#' 
#' @param flag_colors (named `character`)\cr a vector that specifies the colors for `TRUE` and `FALSE`
#'   flag values.
#' @param cor_colors (`function`)\cr color scale function for the correlation values in the heatmap, 
#'   produced by [circlize::colorRamp2()].
#' @param ... other arguments to be passed to [ComplexHeatmap::Heatmap()].
#'   
#' @export
#' 
#' @examples
#' autoplot(result)
#' autoplot(result, show_column_names = FALSE, show_row_names = FALSE)
#'
setMethod(
  f = "autoplot",
  signature = c(object = "HermesDataCor"),
  definition = function(object,
                        flag_colors = c("FALSE" = "green", "TRUE" = "red"),
                        cor_colors = circlize::colorRamp2(c(0, 0.5, 1), c("red", "yellow", "green")),
                        ...) {
    df <- object@flag_data
    left_annotation <- ComplexHeatmap::rowAnnotation(
      LowDepthFlag = factor(df$LowDepthFlag),
      col = list(LowDepthFlag = flag_colors)
    )
    top_annotation <- ComplexHeatmap::HeatmapAnnotation(
      TechnicalFailureFlag = factor(df$TechnicalFailureFlag),
      col = list(TechnicalFailureFlag = flag_colors)
    )
    ComplexHeatmap::Heatmap(
      matrix = object,
      col = cor_colors,
      name = "Correlation",
      left_annotation = left_annotation,
      top_annotation = top_annotation,
      ...
    )
  }
)
