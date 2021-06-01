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

#' Density Plot of (Log) Counts Distributions
#'
#' This creates a density plot of the log2/count distribution of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param log (`logical`)\cr TRUE or FALSE vector for Log2 transformation.
#' @return The `ggplot` object with the density plot.
#' 
#' @export
#' @examples
#' result <- HermesData(summarized_experiment)
#' draw_libsize_densities(result, TRUE)
#'
draw_libsize_densities <- function(object,
                                   log = TRUE){
  assert_that(
    is_class(object, "HermesData"),
    is.logical(log)
  )
  if(isTRUE(log)){
    df <- as.data.frame(log2(counts(object) + 1))
  } else {
    df <- as.data.frame(counts(object))
  }
  df.long <- gather(df)
  ggplot(df.long, aes(value, group = key)) +
    geom_density() +
    expand_limits(x = -2.5) +
    ggtitle(ifelse(isTRUE(log), "Log2 Count Distribution", "Count Distribution")) +
    xlab(ifelse(isTRUE(log), "Log2(Count + 1)", "Counts")) +
    ylab("Density")
}
