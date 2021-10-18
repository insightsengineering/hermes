# pipe ----

#' Pipe operator
#'
#' @description `r lifecycle::badge("stable")`
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Conversion of Character to Factor Variables in a `DataFrame`
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This utility function converts all character variables in a [`S4Vectors::DataFrame`]
#' to factor variables with explicit missing level.
#'
#' @details This is using [tern::df_explicit_na()] which only works for classic [`data.frame`]
#' objects. We avoid a conversion of the whole `data` to [`data.frame`] since that could be
#' problematic when not supported classes are used in other non-character columns.
#'
#' @param data (`DataFrame`)\cr input [`S4Vectors::DataFrame`].
#' @param omit_columns (`character` or `NULL`)\cr which columns should be omitted from
#'   the conversion.
#' @param na_level (`string`)\cr missing level to be used.
#'
#' @return The modified data.
#'
#' @export
#'
#' @examples
#' dat <- colData(summarized_experiment)
#' any(sapply(dat, is.character))
#' dat_converted <- suppressWarnings(df_char_to_factor(dat))
#' any(sapply(dat_converted, is.character))
df_char_to_factor <- function(data,
                              omit_columns = NULL,
                              na_level = "<Missing>") {
  lifecycle::deprecate_warn("0.1.0.9000", "df_char_to_factor()", "df_cols_to_factor()")

  assert_that(is(data, "DataFrame"))
  col_is_char <- sapply(data, is.character)
  if (!any(col_is_char)) {
    return(data)
  }
  data[, col_is_char] <- tern::df_explicit_na(
    as.data.frame(data[, col_is_char]),  # It is safe to convert the character columns only here.
    omit_columns = omit_columns,
    char_as_factor = TRUE,
    na_level = na_level
  )
  data
}

#' Conversion of Character to Factor Variables in a `DataFrame`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This utility function converts all character variables in a [`S4Vectors::DataFrame`]
#' to factor variables with explicit missing level.
#'
#' @details This is using [tern::df_explicit_na()] which only works for classic [`data.frame`]
#' objects. We avoid a conversion of the whole `data` to [`data.frame`] since that could be
#' problematic when not supported classes are used in other non-character columns.
#'
#' @param data (`DataFrame`)\cr input [`S4Vectors::DataFrame`].
#' @param omit_columns (`character` or `NULL`)\cr which columns should be omitted from
#'   the conversion. Note that all required `rowData` and `colData` variables cannot be converted
#'   to ensure proper downstream behavior.
#' @param na_level (`string`)\cr missing level to be used.
#'
#' @return The modified data.
#'
#' @export
#'
#' @examples
#' dat <- colData(summarized_experiment)
#' any(sapply(dat, is.character))
#' any(sapply(dat, is.logical))
#' dat_converted <- df_cols_to_factor(dat)
#' any(sapply(dat_converted, function(x) is.character(x) || is.logical(x)))
df_cols_to_factor <- function(data,
                              omit_columns = NULL,
                              na_level = "<Missing>") {
  assert_that(is(data, "DataFrame"))
  col_is_char_or_logical <- sapply(data, function(x) is.character(x) || is.logical(x))
  if (!any(col_is_char_or_logical)) {
    return(data)
  }
  omit_columns <- union(
    omit_columns,
    c(.row_data_cols, .col_data_cols)
  )
  data[, col_is_char_or_logical] <- tern::df_explicit_na(
    # It is safe to convert the character or logical columns only here.
    as.data.frame(data[, col_is_char_or_logical]),
    omit_columns = omit_columns,
    char_as_factor = TRUE,
    logical_as_factor = TRUE,
    na_level = na_level
  )
  data
}

#' Checks Whether All Missing
#'
#' Internal function to check whether a whole vector is `NA`.
#'
#' @param x (`vector`)\cr vector to check.
#' @return Corresponding flag.
all_na <- function(x) {
  all(is.na(x))
}

#' Make a Short List of a Character Vector
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function makes a short list string, e.g. "a, b, ..., z"
#' out of a character vector, e.g. `letters`.
#'
#' @param x (`character`)\cr input which should be listed.
#' @param sep (`string`)\cr separator to use.
#' @param thresh (`count`)\cr threshold to use, if the length of `x` is larger
#'   then the list will be shortened using the `...` ellipsis.
#'
#' @return String with the short list.
#'
#' @export
#'
#' @examples
#' h_short_list(letters)
#' h_short_list(letters[1:3])
#' h_short_list(LETTERS[1:5], sep= ";", thresh = 5L)
h_short_list <- function(x, sep = ", ", thresh = 3L) {
  assert_character(x)
  assert_string(sep, min.chars = 1L)
  assert_int(thresh, lower = 3L)

  n <- length(x)
  if (n > thresh) {
    x <- c(x[1:2], "...", x[n])
  }
  paste(x, collapse = sep)
}

#' Parenthesize a Character Vector
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function adds parentheses around each element of a character
#' vector.
#'
#' @param x (`character`)\cr inputs which should be parenthesized.
#'
#' @return Character vector with parentheses, except when `x` is a blank string
#'   in which case it is returned unaltered.
#'
#' @export
#'
#' @examples
#' h_parens("bla")
#' h_parens("")
#' h_parens(c("bla", "bli"))
h_parens <- function(x) {
  assert_character(x, any.missing = FALSE)
  ifelse(x == "", x, paste0("(", x, ")"))
}

#' First Principal Component (PC1) Gene Signature
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function returns the first principal component from an assay
#' stored as a `matrix`.
#'
#' @param x (`matrix`)\cr containing numeric data with genes in rows and samples
#'   in columns, no missing values are allowed.
#' @param center (`flag`)\cr whether the variables should be zero centered.
#' @param scale (`flag`)\cr whether the variables should be scaled to have unit variance.
#'
#' @return A numeric vector containing the principal component values for each
#'   column in `x`.
#' @export
#'
#' @examples
#' object <- hermes_data %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize() %>%
#'   assay("counts")
#'
#' colPrinComp1(object)
colPrinComp1 <- function(x,
                         center = TRUE,
                         scale = TRUE) {
  assert_matrix(x, any.missing = FALSE, mode = "numeric")
  assert_flag(center)
  assert_flag(scale)

  gene_is_constant <- apply(x, MARGIN = 1L, FUN = S4Vectors::isConstant)
  selected_data <- x[!gene_is_constant, ]

  pca_result <- stats::prcomp(t(selected_data), center = center, scale = scale)
  pca_result$x[, 1L]
}

#' Mean Z-score Gene Signature
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function returns the Z-score from an assay stored as a `matrix`.
#'
#' @inheritParams colPrinComp1
#'
#' @return A numeric vector containing the mean Z-score values for each
#'   column in `x`.
#' @export
#'
#' @examples
#' object <- hermes_data %>%
#'   add_quality_flags() %>%
#'   filter() %>%
#'   normalize() %>%
#'   assay("counts")
#'
#' colMeanZscores(object)
colMeanZscores <- function(x) {
  assert_matrix(x, any.missing = FALSE, mode = "numeric")

  gene_is_constant <- apply(x, MARGIN = 1L, FUN = S4Vectors::isConstant)
  z_vals <- x
  z_vals[gene_is_constant, ] <- NA
  z_vals[!gene_is_constant, ] <- apply(
    z_vals[!gene_is_constant, , drop = FALSE],
    MARGIN = 1L,
    FUN = scale,  # Note: scale() with centering and scaling is the z-score.
    center = TRUE,
    scale = TRUE
  )
  colMeans(z_vals, na.rm = TRUE)
}

#' Wrap in MAE
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function wraps `SummarizedExperiment` objects into an
#' a `MultiAssayExperiment` (MAE) object.
#'
#' @param x (`SummarizedExperiment`)\cr input to create the MAE object from.
#' @param name (`string`)\cr experiment name to use in the MAE for `x`.
#'
#' @return The MAE object with the only experiment being `x` having the given
#' `name`.
#'
#' @export
#'
#' @examples
#' mae <- wrap_in_mae(summarized_experiment)
#' mae[["summarized_experiment"]]
wrap_in_mae <- function(x,
                        name = deparse(substitute(x))) {
  assert_class(x, "SummarizedExperiment")
  assert_string(name, min.chars = 1L)
  exp_list <- stats::setNames(list(x), name)
  MultiAssayExperiment::MultiAssayExperiment(experiments = exp_list)
}

#' Finding All Duplicates in Vector
#'
#' The difference here to [duplicated()] is that also the first occurrence
#' of a duplicate is flagged as `TRUE`.
#'
#' @inheritParams base::duplicated
#'
#' @return Logical vector flagging all occurrences of duplicate values as `TRUE`.
#' @export
#'
#' @examples
#' h_all_duplicated(c("a", "a", "b"))
#' duplicated(c("a", "a", "b"))
h_all_duplicated <- function(x) {
  front <- duplicated(x, fromLast = FALSE)
  back <- duplicated(x, fromLast = TRUE)
  front | back
}





#' Cut into bins
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function returns a strings corresponding to the interval for each provided observations. Intervals are
#' left-open, right closed.
#'
#' @param x (`numeric` vector)\cr the continuous variable values which should be cut into quantile bins. `NA` values are
#'   not taken into account when computing quantiles and are be attributed to the `NA` interval.
#' @param percentiles (`proportion` vector)\cr the proportions identifying the limits of the intervals
#'   to be generated. Duplicated values are removed.
#' @param digits (`integer`)\cr  the precision to use when formatting the percentages.
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(452)
#' x <- runif(10,-10,10)
#' cut_quantile(x, c(0.33333333,0.6666666), digits = 4)
#'
#' x[1:4] <- NA
#' cut_quantile(x, c(0.2,0.2))
cut_quantile <- function(x,
                         percentiles = c(1/3, 2/3),
                         digits = 1
                         ) {

  assert_that(is.numeric(x))
  assert_that(all(percentiles >= 0), all(percentiles<= 1))
  assert_numeric(digits, lower = 0)

  percentiles_without_borders <- setdiff(percentiles, c(0,1))
  percentiles_without_borders <- unique(percentiles_without_borders)
  percentile_with_borders <- c(0, sort(percentiles_without_borders), 1)

  quant <- quantile(x, percentile_with_borders, names = TRUE, digits = digits, na.rm = TRUE)

  assert_that(!any(duplicated(quant)), msg = "duplicated quantile boundaries")

  name_quant <- names(quant)

  labs_quant <- c()
  for(i in 2:length(name_quant)) {
    labs_quant[i-1] <- paste0(name_quant[i-1], ",", name_quant[i])
  }

  labs_quant <- paste0("(", labs_quant, "]")
  labs_quant[1] <- gsub("\\(", "[", labs_quant[1])

  cut(x, quant, labs_quant, include.lowest = TRUE)

}
