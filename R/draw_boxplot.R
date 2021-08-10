#' Boxplot for Gene Expression Values
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This produces a boxplot of the gene expression values of a gene for a
#'   sample variable.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input.
#' @param x_var (`string`)\cr sample variable for the x-axis.
#' @param y_var (`string`)\cr gene ID for the y-axis.
#' @param color_var (`string` or `NULL`)\cr optional color variable, taken from
#'   input sample variables.
#' @param facet_var (`string` or `NULL`)\cr optional faceting variable, taken
#'   from input sample variables.
#' @param jitter (`flag`)\cr whether to add jittered original data points or not.
#'
#' @return The `ggplot` boxplot.
#'
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = "SEX",
#'   y_var = genes(object)[2],
#'   facet_var = NULL,
#'   color_var = "RACE"
#' )
#'
#' object2 <- object %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize()
#' draw_boxplot(
#'   object2,
#'   assay_name = "tpm",
#'   x_var = "SEX",
#'   y_var = genes(object2)[20],
#'   facet_var = "RACE",
#'   color_var = "AGE18",
#'   jitter = TRUE
#' )
#'
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = "SEX",
#'   y_var = genes(object)[2],
#'   jitter = TRUE
#' )
draw_boxplot <- function(object,
                         assay_name,
                         x_var,
                         y_var,
                         color_var = NULL,
                         facet_var = NULL,
                         jitter = FALSE) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_string(x_var)
  assert_string(y_var)
  assert_string(color_var, null.ok = TRUE)
  assert_string(facet_var, null.ok = TRUE)
  assert_flag(jitter)

  assay_matrix <- assay(object, assay_name)
  col_data <- colData(object)
  assert_names(names(col_data), must.include = x_var)
  assert_names(rownames(assay_matrix), must.include = y_var)
  df <- data.frame(
    x = col_data[, x_var],
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
    geom_boxplot(outlier.shape = ifelse(jitter, NA, 19)) +
    stat_boxplot(geom = "errorbar") +
    labs(x = x_var, y = y_var)
  geom_point_args <- list()
  if (!is.null(color_var)) {
    geom_point_args <- c(geom_point_args, list(
      aes(color = .data$color)
    ))
    p <- p +
      labs(color = color_var)
  }
  if (jitter) {
    geom_point_args <- c(geom_point_args, list(
      position = position_jitter(width = 0.2),
      alpha = 1 / 4
    ))
  }
  p <- p + do.call(geom_point, geom_point_args)
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(~ facet)
  }
  p
}
