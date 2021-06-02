#' Histogram of Library Sizes
#'
#' This creates a histogram of the library sizes of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param bins (`count`)\cr number of evenly distributed groups desired.
#' @param fill (`string`)\cr color of the bars filling.
#' @return The `ggplot` object with the histogram.
#' 
#' @export
#' @examples
#' result <- hermes:::.HermesData(summarized_experiment)
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
  ggplot(df, aes(x = libsize)) +
    geom_histogram(bins = bins, fill = fill) +
    stat_bin(bins = bins, geom = "text", aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.25) +
    ggtitle("Histogram of Library Sizes") +
    xlab("Library Size") +
    ylab("Frequency")
}

#' Q-Q Plot of Library Sizes
#'
#' This creates a Q-Q plot of the library sizes of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param colour (`string`)\cr color of Q-Q line.
#' @param linetype (`string`)\cr linetype of  Q-Q line.
#' @return The `ggplot` object with the histogram.
#' 
#' @export
#' @examples
#' result <- hermes:::.HermesData(summarized_experiment)
#' draw_libsize_qq(result)
#' draw_libsize_qq(result, colour = "blue", linetype = "solid")
#'
draw_libsize_qq <- function(object, 
                            color = "grey",
                            linetype = "dashed") {
  assert_that(
    is_class(object, "AnyHermesData"),
             is.string(colour),
             is.string(linetype)
  )
  df <- data.frame(libsize = colSums(counts(object)))
  
  ggplot(df, aes(sample=.data$libsize)) +
    stat_qq() +
    stat_qq_line(colour=colour, linetype=linetype) +
    theme_classic() +
    ggtitle("QQ plot of Library Sizes") +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.text.y = element_text(angle=90))

}
