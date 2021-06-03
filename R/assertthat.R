#' Additional Assertions for `assert_that`
#'
#' We provide additional assertion functions which can be used together with
#' [assertthat::assert_that()].
#'
#' @name assertions
#' @import assertthat
NULL

# is_class ----

#' @describeIn assertions checks the class.
#' @param object any object.
#' @param class2 (`character` or class definition)\cr the class to which `object` could belong.
#' @export
#' @examples
#' a <- 5
#' is_class(a, "character")
#' 
is_class <- function(object, class2) {
  is(object, class2)
}

on_failure(is_class) <- function(call, env) {
  obj_name <- deparse(call$object)
  class <- eval(call$class2, env)
  paste(obj_name, "is not of class", class)
}

# is_hermes_data ----

#' @describeIn assertions checks the class.
#' @param object any object.
#' @export
#' @examples
#' is_hermes_data(HermesData(summarized_experiment))
#' is_hermes_data(42)
#' 
is_hermes_data <- function(object) {
  is_class(object, "AnyHermesData")
}

on_failure(is_hermes_data) <- function(call, env) {
  obj_name <- deparse(call$object)
  paste(obj_name, "is not a HermesData or RangedHermesData object")
}
