#' Scatterplot for Gene Expression Values
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a scatterplot of two genes or gene signatures.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param x_spec (`GeneSpec`)\cr gene specification for the x-axis.
#' @param y_spec (`GeneSpec`)\cr gene specification for the y-axis.
#' @param color_var (`string` or `NULL`)\cr optional color variable, taken
#'   from input sample variables.
#' @param facet_var (`string` or `NULL`)\cr optional faceting variable, taken
#'   from input sample variables.
#' @param smooth_method (`string`)\cr smoothing method to use, either linear
#'   regression line (`lm`), local polynomial regression (`loess`) or `none`.
#'
#' @return The `ggplot` scatterplot.
#'
#' @export
#'
#' @examples
#' object <- hermes_data
#' g <- genes(object)
#'
#' draw_scatterplot(
#'   object,
#'   assay_name = "counts",
#'   facet_var = NULL,
#'   x_spec = gene_spec(c(A = g[1])),
#'   y_spec = gene_spec(g[2]),
#'   color = "RACE"
#' )
#'
#' object2 <- object %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#' g2 <- genes(object2)
#'
#' draw_scatterplot(
#'   object2,
#'   assay_name = "tpm",
#'   facet_var = "SEX",
#'   x_spec = gene_spec(g2[1:10], colMeans, "Mean"),
#'   y_spec = gene_spec(g2[11:20], colMedians, "Median"),
#'   smooth_method = "loess"
#' )
draw_scatterplot <- function(object,
                             assay_name,
                             x_spec,
                             y_spec,
                             color_var = NULL,
                             facet_var = NULL,
                             smooth_method = c("lm", "loess", "none")) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_class(x_spec, "GeneSpec")
  assert_class(y_spec, "GeneSpec")
  assert_true(x_spec$returns_vector())
  assert_true(y_spec$returns_vector())
  assert_string(color_var, null.ok = TRUE)
  assert_string(facet_var, null.ok = TRUE)
  smooth_method <- match.arg(smooth_method)

  assay_matrix <- assay(object, assay_name)
  col_data <- colData(object)
  df <- data.frame(
    x = x_spec$extract(assay_matrix),
    y = y_spec$extract(assay_matrix)
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
    labs(x = x_spec$get_label(), y = y_spec$get_label())
  if (!is.null(color_var)) {
    p <- p + geom_point(aes(color = .data$color)) +
      labs(color = color_var)
  } else {
    p <- p + geom_point()
  }
  if (smooth_method != "none") {
    p <- p + geom_smooth(method = smooth_method, formula = y ~ x)
  }
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(~facet)
  }
  p
}
