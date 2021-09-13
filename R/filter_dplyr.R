# filter-data.frame ----

#' @rdname filter
setMethod(
  f = "filter",
  signature = signature(object = "data.frame"),
  definition = function(object, ...) {
    # We just forward to the `dplyr` generic function, so that the user
    # can still filter data frames.
    dplyr::filter(object, ...)
  }
)

# filter-ts ----

#' @rdname filter
setMethod(
  f = "filter",
  signature = signature(object = "ts"),
  definition = function(object, ...) {
    # We just forward to the `dplyr` generic function, which in this case
    # will throw a meaningful error and point to `stats::filter`.
    dplyr::filter(object, ...)
  }
)
