# pipe ----

#' Pipe operator
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
#' This utility function converts all character variables in a [`S4Vectors::DataFrame`]
#' to factor variables with explicit missing level.
#'
#' This is using [tern::df_explicit_na()] which only works for classic [`data.frame`] objects. We
#' avoid a conversion of the whole `data` to [`data.frame`] since that could be problematic
#' when not supported classes are used in other non-character columns.
#'
#' @param data (`DataFrame`)\cr input [`S4Vectors::DataFrame`].
#' @param omit_columns (`character` or `NULL`)\cr which columns should be omitted from the conversion.
#' @param na_level (`string`)\cr missing level to be used.
#'
#' @return The modified data.
#'
#' @importFrom tern df_explicit_na
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
