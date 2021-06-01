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




#' Stacked Barplot of Filtered Genes by Chromosome
#'
#' This creates a barplot of filtered genes of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param chrname (`string`)\cr chromosome name.
#' @return The `ggplot` object with the histogram.
#' 
#' @importFrom rlang .data
#' @export
#' @examples
#' load("~/hermes/data/summarized_experiment.rda")
#' object <- HermesData(summarized_experiment)
#' draw_filter_barplot(object)
#'

draw_filter_barplot <- function(object, 
                                chrname = c(as.character(c(1:22)), "X", "Y", "MT", "Others")) {
  assert_that(
    is_class(object, "HermesData"),
    isFALSE(all(is.na(rowData(object)$LowExpressionFlag)))
  )
  
  df <- data.frame(Chromosome = rowData(object)$Chromosome, LowExpressionFlag = rowData(object)$LowExpressionFlag)
  
  df$chr <- factor(ifelse(df$Chromosome %in% chrname, df$Chromosome, "Others"), levels = chrname)
  
  ggplot(data=df, aes(x=chr)) + 
    geom_bar(aes(fill = LowExpressionFlag)) +
    ggtitle("Stacked Barplot of Filtered Genes by Chromosome") +
    xlab("Chromosome") +
    ylab("Number of Genes")
}
