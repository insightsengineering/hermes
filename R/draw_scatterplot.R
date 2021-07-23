#' Scatterplot for Gene Expression Values
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a scatterplot of the gene expression values
#' of two genes.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param x_var (`string`)\cr gene ID for the x-axis.
#' @param y_var (`string`)\cr gene ID for the y-axis.
#' @param color_var (`string` or `NULL`)\cr optional color variable, taken
#'   from input sample variables.
#' @param facet_var (`string` or `NULL`)\cr optional faceting variable, taken
#'   from input sample variables.
#' @param smooth_method (`string`)\cr smoothing method to use, either linear
#'   regression line (`lm`), local polynomial regression (`loess`) or `none`.
#'
#' @return The `ggplot` scatterplot.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' draw_scatterplot(
#'   object,
#'   assay_name = "counts",
#'   facet_var = NULL,
#'   x_var = genes(object)[1],
#'   y_var = genes(object)[2],
#'   color = "RACE"
#' )
#'
#' object2 <- object %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#' draw_scatterplot(
#'   object2,
#'   assay_name = "tpm",
#'   facet_var = "SEX",
#'   x_var = genes(object2)[10],
#'   y_var = genes(object2)[20],
#'   smooth_method = "loess"
#' )
draw_scatterplot <- function(object,
                             assay_name,
                             x_var,
                             y_var,
                             color_var = NULL,
                             facet_var = NULL,
                             smooth_method = c("lm", "loess", "none")) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_string(x_var)
  assert_string(y_var)
  assert_string(color_var, null.ok = TRUE)
  assert_string(facet_var, null.ok = TRUE)
  smooth_method <- match.arg(smooth_method)

  assay_matrix <- assay(object, assay_name)
  col_data <- colData(object)
  df <- data.frame(
    x = assay_matrix[x_var, ],
    y = assay_matrix[y_var, ]
  )
  if (!is.null(facet_var)) {
    assert_names(names(col_data), must.include = facet_var)
    df$facet <- col_data[[facet_var]]
  }
  if (!is.null(color_var)) {
    assert_names(names(col_data), must.include = color_var)
    df$color <- col_data[[color_var]]
  }
  p <- ggplot(df, aes(x = .data$x, y = .data$y)) +
    labs(x = x_var, y = y_var)
  if (!is.null(color_var)) {
    p <- p +
      geom_point(aes(color = .data$color)) +
      labs(color = color_var)
  } else {
    p <- p +
      geom_point()
  }
  if (smooth_method != "none") {
    p <- p +
      geom_smooth(method = smooth_method, formula = y ~ x)
  }
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(~ facet)
  }
  p
}
