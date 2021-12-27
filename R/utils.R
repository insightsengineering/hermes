# pipe ----

#' Pipe operator
#'
#' @description `r lifecycle::badge("stable")`
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @return The result of the corresponding function call.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#'
#' @examples
#' hermes_data %>%
#'   filter() %>%
#'   normalize() %>%
#'   summary()
NULL

#' Conversion to Factors with Explicit Missing Level in a `data.frame`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function converts all character and logical variables
#' to factor variables in a `data.frame`. It also sets an explicit missing data level
#' for all factor variables that have at least one `NA`.
#'
#' @param data (`data.frame`)\cr input data with at least one column.
#' @param na_level (`string`)\cr explicit missing level to be used.
#'
#' @return The modified data.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   a = c(NA, 2),
#'   b = c("A", NA),
#'   c = c("C", "D"),
#'   d = factor(c(NA, "X")),
#'   e = factor(c("Y", "Z"))
#' )
#' h_df_factors_with_explicit_na(dat)
h_df_factors_with_explicit_na <- function(data, na_level = "<Missing>") {
  assert_data_frame(data, min.cols = 1L)
  assert_string(na_level, min.chars = 1L)

  # Conversion of all logical or character variables to factor.
  var_is_char_or_logical <- vapply(data, is.logical, logical(1)) |
    vapply(data, is.character, logical(1))
  data[, var_is_char_or_logical] <- lapply(
    data[, var_is_char_or_logical, drop = FALSE],
    factor
  )

  # Add explicit missing level to all factors that have any `NA`.
  var_is_factor_with_na <- vapply(data, is.factor, logical(1)) &
    vapply(data, anyNA, logical(1))
  data[, var_is_factor_with_na] <- lapply(
    data[, var_is_factor_with_na, drop = FALSE],
    forcats::fct_explicit_na,
    na_level = na_level
  )

  data
}

#' Conversion of Eligible Columns to Factor Variables in a `DataFrame`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This utility function converts all eligible character and logical variables in a
#' [`S4Vectors::DataFrame`] to factor variables. All factor variables get amended
#' with an explicit missing level.
#'
#' @note All required `rowData` and `colData` variables cannot be converted
#'   to ensure proper downstream behavior. These are automatically omitted if found in `data`
#'   and therefore do not need to be specified in `omit_columns`.
#'
#' @param data (`DataFrame`)\cr input [`S4Vectors::DataFrame`].
#' @param omit_columns (`character` or `NULL`)\cr which columns should be omitted from
#'   the possible conversion to factor and explicit missing level application.
#' @param na_level (`string`)\cr explicit missing level to be used for factor variables.
#'
#' @return The modified data.
#'
#' @export
#'
#' @examples
#' dat <- colData(summarized_experiment)
#' any(vapply(dat, is.character, logical(1)))
#' any(vapply(dat, is.logical, logical(1)))
#' dat_converted <- df_cols_to_factor(dat)
#' any(vapply(dat_converted, function(x) is.character(x) || is.logical(x), logical(1)))
df_cols_to_factor <- function(data,
                              omit_columns = NULL,
                              na_level = "<Missing>") {
  assert_that(is(data, "DataFrame"))
  col_is_char_or_logical <- vapply(data, is.character, logical(1)) |
    vapply(data, is.logical, logical(1))
  omit_columns <- union(
    omit_columns,
    c(.row_data_cols, .col_data_cols)
  )
  cols_to_convert <- setdiff(
    names(which(col_is_char_or_logical)),
    omit_columns
  )
  if (length(cols_to_convert)) {
    data[, cols_to_convert] <- h_df_factors_with_explicit_na(
      as.data.frame(data[, cols_to_convert]),
      na_level = na_level
    )
  }
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
#' h_short_list(LETTERS[1:5], sep = ";", thresh = 5L)
h_short_list <- function(x, sep = ", ", thresh = 3L) {
  assert_character(x)
  assert_string(sep, min.chars = 1L)
  assert_int(thresh, lower = 3L)

  n <- length(x)
  if (n > thresh) {
    x <- c(x[c(1L, 2L)], "...", x[n])
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
    FUN = scale, # Note: scale() with centering and scaling is the z-score.
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

#' Cutting a Numeric Vector into a Factor of Quantile Bins
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function transforms a numeric vector into a factor corresponding to the quantile intervals.
#' The intervals are left-open and right-closed.
#'
#' @param x (`numeric`)\cr the continuous variable values which should be cut into quantile bins. `NA` values are
#'   not taken into account when computing quantiles and are attributed to the `NA` interval.
#' @param percentiles (`proportions`)\cr the required percentiles for the quantile intervals
#'   to be generated. Duplicated values are removed.
#' @param digits (`integer`)\cr  the precision to use when formatting the percentages.
#'
#' @return The factor with a description of the available quantiles as levels.
#' @export
#'
#' @examples
#' set.seed(452)
#' x <- runif(10, -10, 10)
#' cut_quantile(x, c(0.33333333, 0.6666666), digits = 4)
#'
#' x[1:4] <- NA
#' cut_quantile(x)
cut_quantile <- function(x,
                         percentiles = c(1 / 3, 2 / 3),
                         digits = 0) {
  assert_numeric(x)
  assert_numeric(percentiles, lower = 0, upper = 1)
  assert_integerish(digits, lower = 0)

  percentiles_without_borders <- setdiff(percentiles, c(0, 1))
  percentiles_without_borders <- round(percentiles_without_borders, digits = digits + 2L)
  percentiles_without_borders <- unique(percentiles_without_borders)
  percentile_with_borders <- c(0, sort(percentiles_without_borders), 1)

  if ((length(x) == 1) && (length(percentile_with_borders) == 2)) {
    return(factor("[0%,100%]"))
  }

  quant <- stats::quantile(
    x,
    probs = percentile_with_borders,
    names = TRUE,
    na.rm = TRUE
  )
  if (any(duplicated(quant))) {
    stop("Duplicate quantiles produced, please use a coarser `percentiles` vector")
  }

  name_quant <- names(quant)
  labs_quant <- paste0(name_quant[-length(name_quant)], ",", name_quant[-1])
  labs_quant <- paste0("(", labs_quant, "]")
  labs_quant[1] <- gsub("\\(", "[", labs_quant[1])


  cut(x, quant, labs_quant, include.lowest = TRUE)
}

#' Concatenate and Print with Newline
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function concatenates inputs like [cat()]
#' and prints them with newline.
#'
#' @seealso This is similar to [cli::cat_line()].
#'
#' @param ... inputs to concatenate.
#'
#' @return None, only used for the side effect of producing the concatenated output in the R console.
#'
#' @export
#'
#' @examples
#' cat_with_newline("hello", "world")
cat_with_newline <- function(...) {
  cat(...)
  cat("\n", append = TRUE)
}
