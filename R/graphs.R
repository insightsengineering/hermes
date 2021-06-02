#' Histogram of Library Sizes
#'
#' This creates a histogram of the library sizes of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param bins (`count`)\cr number of evenly distributed groups desired.
#' @param fill (`string`)\cr color of the bars filling.
#' @return The `ggplot` object with the histogram.
#' 
#' @importFrom rlang .data
#' @export
#' @examples
#' result <- HermesData(summarized_experiment)
#' draw_libsize_hist(result)
#' draw_libsize_hist(result, bins = 10L, fill = "blue")
#'
draw_libsize_hist <- function(object, 
                              bins = 30L,
                              fill = "darkgrey") {
  assert_that(
    is_class(object, "HermesData"),
    is.count(bins),
    is.string(fill)
  )
  df <- data.frame(libsize = colSums(counts(object)))
  ggplot(df, aes(x = .data$libsize)) +
    geom_histogram(bins = bins, fill = fill) +
    stat_bin(
      bins = bins, 
      geom = "text", 
      aes(label = ifelse(.data$..count.. > 0, .data$..count.., "")), 
      vjust = -0.25
    ) +
    ggtitle("Histogram of Library Sizes") +
    xlab("Library Size") +
    ylab("Frequency")
}

#' Boxplot of Non-zero Genes
#' 
#' This draws a boxplot, with overlaid data points, of the number of 
#' non-zero expressed genes per sample.
#'
#' @param object (`HermesData`)\cr input.
#' @param jitter (`number`)\cr `geom_point` aesthetic parameter.
#' @param alpha (`number`)\cr `geom_point` aesthetic parameter.
#'
#' @return The `ggplot` object with the histogram.
#' @export
#' 
#' @examples
#' result <- HermesData(summarized_experiment)
#' draw_nonzero_boxplot(result)
#' draw_nonzero_boxplot(result, jitter = 0.1, alpha = 1/3)
#' 
draw_nonzero_boxplot <- function(object, 
                                 jitter = 0.2,
                                 alpha = 1/4) {
  assert_that(
    is_class(object, "HermesData"),
    is.number(jitter),
    is.number(alpha)
  )
  
  no_na_count <- colSums(counts(object) != 0)
  
  df <- data.frame(no_na = no_na_count, x = "Sample")
  
  ggplot(df, aes(y = .data$no_na, x = .data$x)) +
    geom_boxplot(outlier.shape = NA) +
    stat_boxplot(geom = "errorbar") +
    geom_point(
      position = position_jitter(width = jitter),
      alpha = alpha
    ) +
    stat_n_text(text.box = TRUE) +
    ggtitle("Distribution of non-zero expression genes") +
    xlab("Library") +
    ylab("Number of non-zero genes")
}
