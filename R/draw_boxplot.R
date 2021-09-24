#' Boxplot for Gene Expression Values
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This produces a boxplot of the gene expression values of a gene for a
#' sample variable.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param assay_name (`string`)\cr selects assay from input for the y-axis.
#' @param genes (`GeneSpec`)\cr gene ID(s) for which to produce boxplots.
#' @param x_var (`string` or `NULL`)\cr optional stratifying variable for the x-axis,
#'   taken from input sample variables.
#' @param color_var (`string` or `NULL`)\cr optional color variable, taken from
#'   input sample variables.
#' @param facet_var (`string` or `NULL`)\cr optional faceting variable, taken
#'   from input sample variables.
#' @param violin (`flag`)\cr whether to draw a violin plot instead of a boxplot.
#' @param jitter (`flag`)\cr whether to add jittered original data points or not.
#'
#' @return The `ggplot` boxplot.
#'
#' @export
#'
#' @examples
#' object <- hermes_data
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   genes = genes(object)[2]
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
#'   genes = genes(object2)[20],
#'   facet_var = "RACE",
#'   color_var = "AGE18",
#'   jitter = TRUE
#' )
#'
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = "SEX",
#'   genes = genes(object)[2],
#'   jitter = TRUE
#' )
#'
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = "SEX",
#'   genes = genes(object)[2],
#'   jitter = TRUE,
#'   violin = TRUE
#' )
#'
#' draw_boxplot(
#'   object,
#'   assay_name = "counts",
#'   x_var = "SEX",
#'   genes = genes(object)[1:2],
#'   facet_var = "RACE"
#' )
draw_boxplot <- function(object,
                         assay_name,
                         genes,
                         x_var = NULL,
                         color_var = NULL,
                         facet_var = NULL,
                         violin = FALSE,
                         jitter = FALSE) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_class(genes, "GeneSpec")
  assert_true(genes$returns_vector())
  assert_string(x_var, null.ok = TRUE)
  assert_string(color_var, null.ok = TRUE)
  assert_string(facet_var, null.ok = TRUE)
  assert_flag(violin)
  assert_flag(jitter)

  assay_matrix <- assay(object, assay_name)
  col_data <- colData(object)
  assert_names(rownames(assay_matrix), must.include = genes$get_genes())

  x <- if (!is.null(x_var)) {
    assert_names(names(col_data), must.include = x_var)
    col_data[, x_var]
  } else {
    factor(0)
  }

  df <- data.frame(
    x = x,
    y = genes$extract(assay_matrix),
    fill = factor(rep(genes$get_label(), each = ncol(assay_matrix)))
  )

  if (!is.null(facet_var)) {
    assert_names(names(col_data), must.include = facet_var)
    df$facet <- col_data[[facet_var]]
  }
  if (!is.null(color_var)) {
    assert_names(names(col_data), must.include = color_var)
    df$color <- col_data[[color_var]]
  }
  jitter_width <- if (jitter) NULL else 0
  point_aes <- if (!is.null(color_var)) {
    aes(group = .data$fill, color = .data$color)
  } else {
    aes(group = .data$fill)
  }
  p <- ggplot(df, aes(x = .data$x, y = .data$y, fill = .data$fill)) +
    labs(x = genes$get_label())

  if (!violin) {

    p <- p +
      geom_boxplot(outlier.shape = ifelse(jitter, NA, 19)) +
      stat_boxplot(geom = "errorbar")

  } else {

    p <- p +
      geom_violin(draw_quantiles = c(0.75, 0.5, 0.25))

  }

  p <- p +
    geom_point(
      mapping = point_aes,
      position = position_jitterdodge(jitter.width = jitter_width)
    ) +
    labs(x = x_var, y = assay_name, fill = "Gene")

  if (is.null(x_var)) {
    p <- p +
      scale_x_discrete(breaks = NULL)
  }
  if (!is.null(color_var)) {
    p <- p +
      labs(color = color_var)
  }
  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(~facet)
  }
  p
}
