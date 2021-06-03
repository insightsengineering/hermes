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

# is_counts_vector ----

#' @describeIn assertions checks for a vector of counts (positive integers).
#' @param x vector to check.
#' @export
#' @examples
#' a <- 5
#' is_class(a, "character")
#' 
is_counts_vector <- function(x) {
  is.integer(x) && all(x > 0) && noNA(x) && not_empty(x)
}

on_failure(is_counts_vector) <- function(call, env) {
  x_name <- deparse(call$x)
  paste(x_name, "is not a vector of counts (positive integers)")
}
