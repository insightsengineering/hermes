# draw_libsize_hist() ----

#' Histogram of Library Sizes
#'
#' @param object A [HermesData] object.
#' @param bins Number of evenly distributed groups desired.
#' @importFrom ggplot2 ggplot aes geom_histogram stat_bin ggtitle xlab ylab 
#' @export
#' @examples
#' result <- hermes:::.HermesData(summarized_experiment)
#' draw_libsize_hist(result, 30)
#'
draw_libsize_hist <- function(object, bins){
  df <- as.data.frame(colSums(counts(object)))
  ggplot(df, aes(colSums(counts(object)))) +
    geom_histogram(color = "white", bins = bins) +
    stat_bin(bins = bins, geom = "text", aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.25) +
    ggtitle("Histogram of Library Sizes") +
    xlab("Library Depth") +
    ylab("Frequency")
}