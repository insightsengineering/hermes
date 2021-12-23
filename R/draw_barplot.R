#' Barplot for Gene Expression Percentiles
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a barplot of the dichotomized gene expression counts into two or three
#' categories based on custom defined percentiles.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param x_spec (`GeneSpec`)\cr gene specification for the x-axis.
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
#' object <- hermes_data
#'
#' g <- genes(object)
#'
#' draw_barplot(
#'   object,
#'   assay_name = "counts",
#'   x_spec = gene_spec(g[1]),
#'   facet_var = "SEX",
#'   fill_var = "AGE18"
#' )
#'
#' draw_barplot(
#'   object,
#'   assay_name = "counts",
#'   x_spec = gene_spec(g[1:3], colMedians, "Median"),
#'   facet_var = "SEX",
#'   fill_var = "AGE18"
#' )
#'
#' draw_barplot(
#'   object,
#'   assay_name = "counts",
#'   x_spec = gene_spec(g[1:3], colMeans, "Mean"),
#'   facet_var = "SEX",
#'   fill_var = "AGE18",
#'   percentiles = c(0.1, 0.9)
#' )
draw_barplot <- function(object,
                         assay_name,
                         x_spec,
                         facet_var = NULL,
                         fill_var = NULL,
                         percentiles = c(1 / 3, 2 / 3)) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_class(x_spec, "GeneSpec")
  assert_true(x_spec$returns_vector())
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
  x_cont <- x_spec$extract(assay_matrix)
  col_data <- colData(object)
  df <- data.frame(x = cut_quantile(x_cont, percentiles))
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
    labs(x = x_spec$get_label())
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(~facet)
  }
  if (!is.null(fill_var)) {
    p <- p + aes(fill = .data$fill) +
      labs(fill = fill_var)
  }
  p
}
