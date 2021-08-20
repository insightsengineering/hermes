#' Barplot for Gene Expression Percentiles
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a barplot of the dichotomized gene expression counts into two or three
#' categories based on custom defined percentiles.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param x_var (`string`)\cr gene ID for the x-axis.
#' @param facet_var (`string` or `NULL`)\cr optional faceting variable, taken
#'   from input sample variables.
#' @param fill_var (`string` or `NULL`)\cr optional fill variable, taken
#'   from input sample variables.
#' @param percentiles (`vector`)\cr lower and upper percentiles to dichotomize
#'   the gene counts into two or three categories.
#'
#' @return The `ggplot` barplot.
#'
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment) %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#'
#' draw_barplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = genes(object)[1],
#'   facet_var = "SEX",
#'   fill_var = "AGE18"
#' )
#'
#' draw_barplot(
#'   object,
#'   assay_name = "tpm",
#'   x_var = genes(object)[5],
#'   percentiles = c(0.2, 0.8)
#' )
#'
#' draw_barplot(
#'   object,
#'   assay_name = "cpm",
#'   x_var = genes(object)[1],
#'   facet_var = "SEX",
#'   fill_var = "AGE18",
#'   percentiles = c(0, 0.8)
#' )
draw_barplot <- function(object,
                         assay_name,
                         x_var,
                         facet_var = NULL,
                         fill_var = NULL,
                         percentiles = c(1/3, 2/3)) {
  assert_class(object, "HermesData")
  assert_string(assay_name)
  assert_string(x_var)
  assert_string(facet_var, null.ok = TRUE)
  assert_string(fill_var, null.ok = TRUE)
  assert_numeric(
    percentiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    len = 2L,
    unique = TRUE,
    sorted = TRUE
  )

  assay_matrix <- assay(object, assay_name)
  assert_names(genes(object), must.include = x_var)
  x_vals <- assay_matrix[x_var, ]
  percentiles_without_borders <- setdiff(percentiles, c(0, 1))
  df <- data.frame(
    x = tern::cut_quantile_bins(x_vals, probs = percentiles_without_borders)
  )

  col_data <- colData(object)
  if (!is.null(facet_var)) {
    assert_names(names(col_data), must.include = facet_var)
    df$facet <- col_data[[facet_var]]
  }
  if (!is.null(fill_var)) {
    assert_names(names(col_data), must.include = fill_var)
    df$fill <- col_data[[fill_var]]
  }

  p <- ggplot(df, aes(x = .data$x)) +
    geom_bar() +
    labs(x = x_var)
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(~ facet)
  }
  if (!is.null(fill_var)) {
    p <- p +
      aes(fill = .data$fill) +
      labs(fill = fill_var)
  }
  p
}
