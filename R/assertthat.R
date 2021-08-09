#' Additional Assertions for `assert_that`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional assertion functions which can be used together with
#' [assertthat::assert_that()].
#'
#' @param x an object to check.
#'
#' @name assertions
#' @import assertthat
NULL

# is_class ----

#' @describeIn assertions checks the class.
#' @param class2 (`character` or class definition)\cr the class to which `x` could belong.
#' @export
#' @examples
#' # Assert a general class.
#' a <- 5
#' is_class(a, "character")
is_class <- function(x, class2) {
  is(x, class2)
}

on_failure(is_class) <- function(call, env) {
  obj_name <- deparse(call$x)
  class <- eval(call$class2, env)
  paste(obj_name, "is not of class", class)
}

# is_hermes_data ----

#' @describeIn assertions checks whether `x` is an [`AnyHermesData`] object.
#' @export
#' @examples
#'
#' # Assert a `AnyHermesData` object.
#' is_hermes_data(HermesData(summarized_experiment))
#' is_hermes_data(42)
is_hermes_data <- function(x) {
  is_class(x, "AnyHermesData")
}

on_failure(is_hermes_data) <- function(call, env) {
  obj_name <- deparse(call$x)
  paste(obj_name, "is not a HermesData or RangedHermesData object")
}

# is_counts_vector ----

#' @describeIn assertions checks for a vector of counts (positive integers).
#' @export
#' @examples
#'
#' # Assert a counts vector.
#' a <- 5L
#' is_counts_vector(a)
is_counts_vector <- function(x) {
  is.integer(x) && all(x > 0) && noNA(x) && not_empty(x)
}

on_failure(is_counts_vector) <- function(call, env) {
  x_name <- deparse(call$x)
  paste(x_name, "is not a vector of counts (positive integers)")
}

# is_list_with ----

#' @describeIn assertions checks for a list containing elements.
#' @param elements (`character`)\cr names of elements which should be in the list `x`.
#'
#' @export
#' @examples
#'
#' # Assert a list containing certain elements.
#' b <- list(a = 5, b = 3)
#' is_list_with(b, c("a", "c"))
#' is_list_with(b, c("a", "b"))
is_list_with <- function(x, elements) {
  assert_that(utils.nest::is_character_vector(elements))
  utils.nest::is_fully_named_list(x) &&
    all(elements %in% names(x))
}

on_failure(is_list_with) <- function(call, env) {
  x_name <- deparse(call$x)
  elements <- eval(call$elements, env)
  paste(
    x_name, "is not a fully and uniquely named list containing all elements",
    paste(elements, collapse = ", ")
  )
}

# one_provided ----

#' @describeIn assertions checks that exactly one of the two inputs `one`, `two` is not `NULL`.
#' @param one first input.
#' @param two second input.
#' @export
#'
#' @examples
#'
#' # Assert that exactly one of two arguments is provided.
#' a <- 10
#' b <- 10
#' one_provided(a, b)
#' one_provided(a, NULL)
one_provided <- function(one, two) {
  (is.null(one) && !is.null(two)) ||
    (is.null(two) && !is.null(one))
}

on_failure(one_provided) <- function(call, env) {
  one_name <- deparse(call$one)
  two_name <- deparse(call$two)
  paste(
    "only one of", one_name, "and", two_name,
    "must be specified, the other needs to be set to NULL"
  )
}

# is_constant ----

#' @describeIn assertions checks whether the vector `x` is constant (only supports `numeric`, `factor`,
#'   `character`, `logical`). `NA`s are removed first.
#' @export
#'
#' @examples
#'
#' # Assert a constant vector.
#' is_constant(c(1, 2))
#' is_constant(c(NA, 1))
#' is_constant(c("a", "a"))
is_constant <- function(x) {
  assert_that(is.vector(x))
  x <- x[!is.na(x)]
  if (is.numeric(x)) {
    isConstant(x)
  } else if (is.factor(x)) {
    isConstant(as.integer(x))
  } else if (is.character(x)) {
    identical(length(unique(x)), 1L)
  } else if (is.logical(x)) {
    all(x) || all(!x)
  } else {
    stop("not supported type")
  }
}
