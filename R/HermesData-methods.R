#' Row Binding of HermesData Objects
#'
#' This method combines HermesData objects with the same samples but different
#' features of interest (rows in assays).
#'
#' @details Note that this is used implicitly via the generic [base::rbind()]
#'   function, see [methods::rbind2()] for more information about the
#'   dispatching details.
#' @aliases rbind
#'
#' @param x (`HermesData`)\cr upper object.
#' @param y (`HermesData`)\cr lower object.
#'
#' @return The combined [HermesData] object.
#' @export
#'
#' @examples
#' # todo
#' 
setMethod(
  f = "rbind2", 
  c("HermesData", "HermesData"),
  function(x, y, ...) {
    args <- list(x, y)
    SummarizedExperiment:::.rbind.SummarizedExperiment(args)
  }
)

.rbind2.error <- function(x, y, ...) {
  stop("one argument is not of class HermesData, please first convert it")
}

#' @export
setMethod(f = "rbind2", c("ANY", "HermesData"), .rbind2.error)

#' @export
setMethod(f = "rbind2", c("HermesData", "ANY"), .rbind2.error)
