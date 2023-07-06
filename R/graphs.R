#' Histogram of Library Sizes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This creates a histogram of the library sizes of the [AnyHermesData] object.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param bins (`count`)\cr number of evenly distributed groups desired.
#' @param fill (`string`)\cr color of the bars filling.
#' @return The `ggplot` object with the histogram.
#'
#' @export
#' @examples
#' result <- hermes_data
#' draw_libsize_hist(result)
#' draw_libsize_hist(result, bins = 10L, fill = "blue")
draw_libsize_hist <- function(object,
                              bins = 30L,
                              fill = "darkgrey") {
  assert_that(
    is_hermes_data(object),
    is.count(bins),
    is.string(fill)
  )
  df <- data.frame(libsize = colSums(counts(object)))
  ggplot(df, aes(x = .data$libsize)) +
    geom_histogram(bins = bins, fill = fill) +
    stat_bin(
      bins = bins,
      geom = "text",
      aes(label = ifelse(after_stat(.data$count) > 0, after_stat(.data$count), "")),
      vjust = -0.25
    ) +
    ggtitle("Histogram of Library Sizes") +
    xlab("Library Size") +
    ylab("Frequency")
}

#' Q-Q Plot of Library Sizes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This creates a Q-Q plot of the library sizes of the [AnyHermesData] object.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param color (`string`)\cr color of Q-Q line.
#' @param linetype (`string`)\cr line type of  Q-Q line.
#' @return The `ggplot` object with the Q-Q Plot.
#'
#' @export
#' @examples
#' result <- hermes_data
#' draw_libsize_qq(result)
#' draw_libsize_qq(result, color = "blue", linetype = "solid")
#'
#' # We can also add sample names as labels.
#' library(ggrepel)
#' draw_libsize_qq(result) + geom_text_repel(label = colnames(result), stat = "qq")
draw_libsize_qq <- function(object,
                            color = "grey",
                            linetype = "dashed") {
  assert_that(
    is_hermes_data(object),
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
#' @description `r lifecycle::badge("experimental")`
#'
#' This creates a density plot of the (log) counts distributions of the [AnyHermesData] object where each line
#' on the plot corresponds to a sample.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param log (`flag`)\cr should the counts be log transformed (log2).
#' @return The `ggplot` object with the density plot.
#'
#' @export
#' @examples
#' result <- hermes_data
#' draw_libsize_densities(result)
#' draw_libsize_densities(result, log = FALSE)
draw_libsize_densities <- function(object,
                                   log = TRUE) {
  assert_that(
    is_hermes_data(object),
    is.flag(log)
  )
  counts <- as.data.frame(counts(object))
  if (isTRUE(log)) {
    df <- log2(counts + 1)
    title <- "Log2 Count Distribution"
    xlab <- "Log2(Count + 1)"
  } else {
    df <- counts
    title <- "Count Distribution"
    xlab <- "Counts"
  }
  df_long <- tidyr::gather(df, key = "Sample", value = "Counts")
  ggplot(df_long, aes(.data$Counts, group = .data$Sample)) +
    geom_density() +
    expand_limits(x = -2.5) +
    ggtitle(title) +
    xlab(xlab) +
    ylab("Density")
}

#' Boxplot of Non-Zero Genes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This draws a boxplot, with overlaid data points, of the number of
#' non-zero expressed genes per sample.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param position (`Position`)\cr specifies x-axis position of points, e.g. for jittering.
#' @param alpha (`proportion`)\cr specifies transparency of points.
#'
#' @return The `ggplot` object with the boxplot.
#'
#' @export
#'
#' @examples
#' # Default boxplot.
#' result <- hermes_data
#' draw_nonzero_boxplot(result)
#'
#' # Reusing the same position for labeling.
#' library(ggrepel)
#' pos <- position_jitter(0.5)
#' draw_nonzero_boxplot(result, position = pos) +
#'   geom_text_repel(aes(label = samples(result)), position = pos)
draw_nonzero_boxplot <- function(object,
                                 position = position_jitter(0.2),
                                 alpha = 0.25) {
  assert_that(
    is_hermes_data(object),
    is_class(position, "Position")
  )
  expect_proportion(alpha)

  no_na_count <- colSums(counts(object) != 0)
  df <- data.frame(no_na = no_na_count, x = "Sample")

  ggplot(df, aes(y = .data$no_na, x = .data$x)) +
    geom_boxplot(outlier.shape = NA) +
    stat_boxplot(geom = "errorbar") +
    geom_point(
      position = position,
      alpha = alpha
    ) +
    EnvStats::stat_n_text(text.box = TRUE) +
    ggtitle("Distribution of non-zero expressed genes") +
    xlab("Library") +
    ylab("Number of non-zero genes")
}

#' Stacked Barplot of Low Expression Genes by Chromosome
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This creates a barplot of chromosomes for the [AnyHermesData] object with the proportions of low expression genes.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param chromosomes (`character`)\cr names of the chromosomes which should be displayed.
#' @param include_others (`flag`)\cr option to show the chromosomes not in `chromosomes` as "Others".
#' @return The `ggplot` object with the histogram.
#'
#' @export
#' @examples
#' object <- hermes_data
#'
#' # Display chromosomes 1-22, X, Y, and MT. Other chromosomes are displayed in "Others".
#' # To increase readability, we can have flip the coordinate axes.
#' draw_genes_barplot(object) + coord_flip()
#'
#' # Alternatively we can also rotate the x-axis tick labels.
#' draw_genes_barplot(object) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#'
#' # Display chromosomes 1 and 2. Other chromosomes are displayed in "Others".
#' draw_genes_barplot(object, chromosomes = c("1", "2"))
#'
#' # Display chromosomes 1 and 2 only.
#' draw_genes_barplot(object, chromosomes = c("1", "2"), include_others = FALSE)
draw_genes_barplot <- function(object,
                               chromosomes = c(seq_len(22), "X", "Y", "MT"),
                               include_others = TRUE) {
  assert_that(
    is_hermes_data(object),
    noNA(rowData(object)$low_expression_flag),
    is.flag(include_others),
    !any(duplicated(chromosomes)),
    !("Others" %in% chromosomes)
  )

  df <- data.frame(
    chromosome = rowData(object)$chromosome,
    low_expression_flag = rowData(object)$low_expression_flag,
    stringsAsFactors = FALSE
  )

  df$chr <- factor(
    ifelse(df$chromosome %in% chromosomes, df$chromosome, "Others"),
    levels = c(chromosomes, "Others")
  )

  if (!include_others) df <- df[df$chr != "Others", ]

  ggplot(data = df, aes(x = .data$chr)) +
    geom_bar(aes(fill = .data$low_expression_flag)) +
    ggtitle("Stacked Barplot of Filtered Genes by Chromosome") +
    xlab("Chromosome") +
    ylab("Number of Genes") +
    labs(fill = "Low Expression")
}

# autoplot(AnyHermesData) ----

#' All Standard Plots in Default Setting
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This generates all standard plots - histogram and q-q plot of library sizes, density plot of the (log) counts
#' distributions, boxplot of the number of number of non-zero expressed genes per sample, and a stacked barplot of low
#' expression genes by chromosome at default setting.
#'
#' @rdname plot_all
#' @aliases plot_all
#'
#' @param object (`AnyHermesData`)\cr input.
#'
#' @return A list with the `ggplot` objects from [draw_libsize_hist()], [draw_libsize_qq()],
#'   [draw_libsize_densities()], [draw_nonzero_boxplot()] and [draw_genes_barplot()]
#'   functions with default settings.
#' @export
#'
#' @examples
#' result <- hermes_data
#' autoplot(result)
setMethod(
  f = "autoplot",
  signature = c(object = "AnyHermesData"),
  definition = function(object) {
    assert_that(
      is_hermes_data(object)
    )
    result <- list(
      libsize_hist = draw_libsize_hist(object),
      libsize_qq = draw_libsize_qq(object),
      libsize_densities = draw_libsize_densities(object),
      nonzero_boxplot = draw_nonzero_boxplot(object),
      genes_barplot = draw_genes_barplot(object)
    )
    lapply(
      X = result,
      FUN = grid::grid.draw
    )
    invisible(result)
  }
)
