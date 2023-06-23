# assertion_arguments ----

#' Standard Assertion Arguments
#'
#' The documentation to this function lists all the conventional arguments in
#' additional `checkmate` assertions.
#'
#' @param x an object to check.
#' @param null.ok (`flag`)\cr whether `x` may also be `NULL`.
#' @param .var.name (`string`)\cr name of the checked object to print in
#'   assertions; defaults to the heuristic implemented in [checkmate::vname()].
#' @param add (`AssertCollection` or `NULL`)\cr collection to store
#'   assertion messages, see [`checkmate::AssertCollection`].
#' @param info (`string`)\cr extra information to be included in the
#'   message for the `testthat` reporter, see [testthat::expect_that()].
#' @param label (`string`)\cr name of the checked object to print in
#'   messages. Defaults to the heuristic implemented in [checkmate::vname()].
#'
#' @name assertion_arguments
NULL
