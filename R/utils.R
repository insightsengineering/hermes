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
#' dat_converted <- df_char_to_factor(dat)
#' any(sapply(dat_converted, is.character))
df_char_to_factor <- function(data,
                              omit_columns = NULL,
                              na_level = "<Missing>") {
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

#' Parenthesize a String
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function adds parentheses around a string.
#'
#' @param x (`string`)\cr input which should be parenthesized.
#'
#' @return String with parentheses, except when `x` is a blank string
#'   in which case it is returned unaltered.
#'
#' @export
#'
#' @examples
#' h_parens("bla")
#' h_parens("")
h_parens <- function(x) {
  assert_string(x)
  if (identical(x, ""))
    ""
  else
    paste0("(", x, ")")
}










#' Generate gene signature using PC1
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function returns the PC1 from an assay stored in an
#' object of class [`Hermes::HermesData`]
#'
#'
#' @param x object (`HermedData`)
#' @param x_assay (`string`) name of the assay to use
#' @param center (`logical`) should the variables be zero centered
#' @param scale (`logical`) should the variables be scaled to have unit variance
#'
#' @return A named [`vector`] containing the value of PC1
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment) |>
#'   add_quality_flags() |>
#'   filter() |>
#'   normalize()
#'
#' colPrinComp1(object,"counts")
colPrinComp1 <- function(x,
                         x_assay = "counts",
                         center = TRUE,
                         scale = TRUE){

  assert_that(
    is_hermes_data(x),
    x_assay %in% assayNames(x),
    is.logical(center),
    is.logical(scale)
  )

  # identify 0 variance genes
  cst_dim = apply(
    assay(x,x_assay),
    1,
    \(x) sd(x) != 0
  )

  # identify genes without missing values (prcomp does not tolerate NAs)
  complete_dim = apply(
    assay(x,x_assay),
    1,
    \(x) !any(is.na(x))
  )

  selected_dim = cst_dim & complete_dim

  selected_data = assay(x,x_assay)[selected_dim,]

  prcomp(t(selected_data), center = center, scale = scale)$x[,1]

}







#' Generate gene signature using mean Z-score
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function returns the mean Z-score from an assay stored in an
#' object of class [`Hermes::HermesData`]
#'
#'
#' @param x object (`HermedData`)
#' @param x_assay (`string`) name of the assay to use
#'
#' @return A named [`vector`] containing the mean Z-score
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment) |>
#'   add_quality_flags() |>
#'   filter()
#'
#' colMeanZscores(object,"counts")
colMeanZscores <- function(x,
                         x_assay = "counts"){

  assert_that(
    is_hermes_data(x),
    x_assay %in% assayNames(x)
  )

  zmat = apply(
    assay(x,x_assay),
    1,
    \(x) if(sd(x)>0) scale(x) else rep(NA,length(x))
  )

  zmean <- apply(
    zmat,
    1,
    mean,
    na.rm = TRUE
  )

  names(zmean) <- colnames(x)

  zmean

}








