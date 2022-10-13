#' Heatmap for Gene Expression Counts
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a heatmap of the chosen assay and groups by various sample variables.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param color_extremes (`numeric`)\cr min and max percentiles to inform the color scheme
#'   of the heatmap as blue and red respectively.
#' @param col_data_annotation (`character` or `NULL`)\cr optional grouping variable(s), taken
#'   from input sample variables.
#' @param ... additional arguments to pass to [ComplexHeatmap::Heatmap()].
#'
#' @return The `ComplexHeatmap::Heatmap` heatmap
#'
#' @export
#'
#' @examples
#' result <- hermes_data %>%
#'   normalize(methods = "voom") %>%
#'   add_quality_flags() %>%
#'   filter(what = "genes")
#'
#' draw_heatmap(
#'   object = result[1:10, ],
#'   assay_name = "counts",
#'   col_data_annotation = "COUNTRY"
#' )
#'
#' draw_heatmap(
#'   object = result[1:10, ],
#'   assay_name = "counts",
#'   color_extremes = c(0.001, 0.999),
#'   col_data_annotation = "AGEGRP"
#' )
draw_heatmap <- function(object,
                         assay_name,
                         color_extremes = c(0.01, 0.99),
                         col_data_annotation = NULL,
                         ...) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_numeric(
    color_extremes,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    len = 2L,
    unique = TRUE,
    sorted = TRUE
  )
  assert_string(col_data_annotation, null.ok = TRUE)

  selected_assay <- assay(object, assay_name)
  color_probs <- c(color_extremes[1L], 0.5, color_extremes[2L])
  color_quantiles <- stats::quantile(x = selected_assay, probs = color_probs)
  colors <- circlize::colorRamp2(
    color_quantiles,
    c("blue", "white", "red")
  )

  if (!is.null(col_data_annotation)) {
    assert_character(col_data_annotation, any.missing = FALSE)
    cd <- colData(object)
    assert_names(names(cd), must.include = col_data_annotation)
    df <- cd[col_data_annotation]
    sample_annotation <- ComplexHeatmap::HeatmapAnnotation(df = df)
    sample_order <- order(df)
  } else {
    sample_annotation <- sample_order <- NULL
  }

  ComplexHeatmap::Heatmap(
    selected_assay,
    name = assay_name,
    col = colors,
    top_annotation = sample_annotation,
    column_order = sample_order,
    ...
  )
}
