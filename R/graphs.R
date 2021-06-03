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
#'
#' Q-Q Plot of Library Sizes
#'
#' This creates a Q-Q plot of the library sizes of the [HermesData] object.
#'
#' @param object (`HermesData`)\cr input.
#' @param color (`string`)\cr color of Q-Q line.
#' @param linetype (`string`)\cr linetype of  Q-Q line.
#' @return The `ggplot` object with the Q-Q Plot.
#' 
#' @export
#' @examples
#' result <- HermesData(summarized_experiment)
#' draw_libsize_qq(result)
#' draw_libsize_qq(result, color = "blue", linetype = "solid")
#'
draw_libsize_qq <- function(object, 
                            color = "grey",
                            linetype = "dashed") {
  assert_that(
    is_class(object, "AnyHermesData"),
    is.string(color),
    is.string(linetype)
  )
  df <- data.frame(libsize = colSums(counts(object)))
  ggplot(df, aes(sample = .data$libsize)) +
    stat_qq() +
    stat_qq_line(color = color, linetype = linetype) +
    theme_classic() +
    ggtitle("Q-Q Plot of Library Sizes") +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.text.y = element_text(angle = 90))
}

#' Density Plot of (Log) Counts Distributions
#'
#' This creates a density plot of the (log) counts distributions of the [HermesData] object where each line
#' on the plot corresponds to a sample. 
#'
#' @param object (`HermesData`)\cr input.
#' @param log (`flag`)\cr should the counts be log transformed (log2).
#' @return The `ggplot` object with the density plot.
#' 
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @export
#' @examples
#' result <- HermesData(summarized_experiment)
#' draw_libsize_densities(result)
#' draw_libsize_densities(result, FALSE)
#'
draw_libsize_densities <- function(object,
                                   log = TRUE){
  assert_that(
    is_class(object, "HermesData"),
    is.flag(log)
  )
  counts <- as.data.frame(counts(object))
  if(isTRUE(log)){
    df <- log2(counts + 1)
    title <- "Log2 Count Distribution"
    xlab <- "Log2(Count + 1)"
  } else {
    df <- counts
    title <- "Count Distribution"
    xlab <- "Counts"
  }
  df.long <- gather(df, key = "Sample", value = "Counts")
  ggplot(df.long, aes(.data$Counts, group = .data$Sample)) +
    geom_density() +
    expand_limits(x = -2.5) +
    ggtitle(title) +
    xlab(xlab) +
    ylab("Density")
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
    ggtitle("Distribution of non-zero expressed genes") +
    xlab("Library") +
    ylab("Number of non-zero genes")
}

#' Stacked Barplot of Low Expression Genes by Chromosome
#'
#' This creates a barplot of chromosomes for the [HermesData] object with the proportions of low expression genes.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param chromosomes (`character`)\cr names of the chromosomes which should be displayed. 
#' @param include_others (`logical`)\cr option to show the chromosomes not in namelist as "others". The default is TRUE.
#' @return The `ggplot` object with the histogram.
#' 
#' @importFrom rlang .data
#' @export
#' @examples
#' object <- HermesData(summarized_experiment)
#' 
#' # Display chromosomes 1-22, X, Y, and MT. Other chromosomes are displayed in "others".
#' draw_genes_barplot(object)
#'
#' # Display chromosomes 1 and 2. Other chromosomes are displayed in "others".
#' draw_genes_barplot(object, chromosomes = c("1", "2"))
#' 
#' # Display chromosomes 1 and 2 only.
#' draw_genes_barplot(object, chromosomes = c("1", "2"), include_others = FALSE)
#' 
draw_genes_barplot <- function(object, 
                               chromosomes = c(as.character(c(1:22)), "X", "Y", "MT"),
                               include_others = TRUE) {
  assert_that(
    is_class(object, "HermesData"),
    noNA(rowData(object)$LowExpressionFlag)
  )
  
  df <- data.frame(Chromosome = rowData(object)$Chromosome, 
                   LowExpressionFlag = rowData(object)$LowExpressionFlag, 
                   stringsAsFactors = FALSE)
  
  chromosomes <- unique(c(chromosomes, "Others"))
  df$chr <- factor(ifelse(df$Chromosome %in% chromosomes, df$Chromosome, "Others"), levels = chromosomes)
  
  if(isFALSE(include_others)) df <- subset(df, df$chr != "Others")
  
  ggplot(data = df, aes(x = .data$chr)) + 
    geom_bar(aes(fill = .data$LowExpressionFlag)) +
    ggtitle("Stacked Barplot of Filtered Genes by Chromosome") +
    xlab("Chromosome") +
    ylab("Number of Genes")
}  