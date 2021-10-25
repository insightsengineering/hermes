#' @include HermesData-methods.R
NULL

# correlate-AnyHermesData ----

#' Correlation between Sample Counts of `AnyHermesData`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `correlate()` method can calculate the correlation matrix between the sample vectors of
#' counts from a specified assay. This produces a [`HermesDataCor`] object, which is an extension
#' of a [`matrix`] with additional quality flags in the slot `flag_data`
#' (containing the `tech_failure_flag` and `low_depth_flag` columns describing the original
#' input samples).
#'
#' An `autoplot()` method then afterwards can produce the corresponding heatmap.
#'
#' @rdname calc_cor
#' @aliases calc_cor
#'
#' @param object (`AnyHermesData`)\cr object to calculate the correlation.
#' @param assay_name (`string`)\cr the name of the assay to use.
#' @param method (`string`)\cr the correlation method, see [stats::cor()] for details.
#'
#' @return A [`HermesDataCor`] object.
#'
#' @export
#'
#' @examples
#' object <- hermes_data
#'
#' # Calculate the sample correlation matrix.
#' correlate(object)
#'
#' # We can specify another correlation coefficient to be calculated.
#' result <- correlate(object, method = "spearman")
setMethod(
  f = "correlate",
  signature = "AnyHermesData",
  definition = function(object,
                        assay_name = "counts",
                        method = "pearson",
                        ...) {
    assert_that(is.string(assay_name))
    chosen_assay <- assay(object, assay_name)
    sample_cor_matrix <- stats::cor(chosen_assay, method = method)

    .HermesDataCor(
      sample_cor_matrix,
      flag_data = colData(object)[, c("tech_failure_flag", "low_depth_flag")]
    )
  }
)

# HermesDataCor ----

#' @rdname calc_cor
#' @aliases HermesDataCor
#' @exportClass HermesDataCor
#'
.HermesDataCor <- setClass( # nolint
  Class = "HermesDataCor",
  contains = "matrix",
  slots = c(flag_data = "DataFrame")
)

# autoplot-HermesDataCor ----

#' @describeIn calc_cor This `autoplot()` method uses the [ComplexHeatmap::Heatmap()] function
#'   to plot the correlations between samples saved in a [`HermesDataCor`] object.
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
#'
#' # Plot the correlation matrix.
#' autoplot(result)
#'
#' # We can customize the heatmap.
#' autoplot(result, show_column_names = FALSE, show_row_names = FALSE)
#'
#' # Including changing the axis label text size.
#' autoplot(
#'   result,
#'   row_names_gp = grid::gpar(fontsize = 8),
#'   column_names_gp = grid::gpar(fontsize = 8)
#' )
setMethod(
  f = "autoplot",
  signature = c(object = "HermesDataCor"),
  definition = function(object,
                        flag_colors = c("FALSE" = "green", "TRUE" = "red"),
                        cor_colors = circlize::colorRamp2(c(0, 0.5, 1), c("red", "yellow", "green")),
                        ...) {
    df <- object@flag_data
    left_annotation <- ComplexHeatmap::rowAnnotation(
      "Low Depth" = factor(df$low_depth_flag),
      col = list("Low Depth" = flag_colors)
    )
    top_annotation <- ComplexHeatmap::HeatmapAnnotation(
      "Technical Failure" = factor(df$tech_failure_flag),
      col = list("Technical Failure" = flag_colors)
    )
    mat <- as(object, "matrix")
    ComplexHeatmap::Heatmap(
      matrix = mat,
      col = cor_colors,
      name = "Correlation",
      left_annotation = left_annotation,
      top_annotation = top_annotation,
      ...
    )
  }
)
