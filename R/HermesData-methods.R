#' HermesData Methods
#' 
#' @description Combines HermesData objects with the same samples but differnet features of interest (rows in assays).
#' 
#' @param args HermesData
#'
#' @return A HermesData
#' @export 
#'
#' @examples

setMethod(f = "rbind", 
          "HermesData",
          function(..., deparse.level = 1)
          {
           args <- unname(list(...)) #removes names and dimnames of the object
           .rbind.HermesData(args)
          })

# rbind function ----

.rbind.HermesData <- function(args)
  {
  SE_check <- unlist(lapply(args, function(x) "HermesData" %in% class(x)))
  if (!all(SE_check)) stop("Not all input objects are HermesData, please check again")

  SummarizedExperiment:::.rbind.SummarizedExperiment(args)
}