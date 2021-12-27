#' Additional Assertions for `checkmate`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional assertion functions which can be used together with
#' the `checkmate` functions. These are described in individual help pages
#' linked below.
#'
#' @return Depending on the function prefix.
#' - `assert_` functions return the object invisibly if successful, and otherwise
#'   throw an error message.
#' - `check_` functions return `TRUE` if successful, otherwise a string with the
#'   error message.
#' - `test_` functions just return `TRUE` or `FALSE`.
#'
#' @seealso [assert_proportion()]
#'
#' @name assertions
#' @import checkmate
NULL

# assert_proportion ----

#' Check for proportion
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Check whether `x` is a (single) proportion.
#'
#' @inheritParams assertion_arguments
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#'
#' @examples
#' check_proportion(0.25)
check_proportion <- function(x, null.ok = FALSE) {
  ok <- test_number(x, lower = 0, upper = 1)
  if (!ok) {
    return("Must be a 'proportion': number between 0 and 1")
  }
  return(TRUE)
}

#' @rdname check_proportion
#' @inheritParams assertion_arguments
#' @export
assert_proportion <- makeAssertionFunction(check_proportion)

#' @rdname check_proportion
#' @export
test_proportion <- makeTestFunction(check_proportion)

#' @rdname check_proportion
#' @inheritParams assertion_arguments
#' @export
expect_proportion <- makeExpectationFunction(check_proportion)
