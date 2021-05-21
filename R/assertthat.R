#' Additional Assertions for `assert_that`
#'
#' We provide additional assertion functions which can be used together with [assertthat::assert_that()].
#'
#' @name assertions
#' @import assertthat
NULL

# on_failure(is) ----
#' @describeIn assertions Failure message for is() function to get class assertions
#' @export
#' @examples
#' 
#' a <- 5
#' assert_that(is(a, "character"))
#' "Error: a is not of class character"
 
assertthat::on_failure(is) <- function(call, env) {
  obj_name <- deparse(call$object)
  class <- eval(call$class2, env)
  paste(obj_name, "is not of class", class)
}
