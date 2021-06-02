# control_normalize ----

#' Settings for the normalize function
#' 
#' @param log (`flag`)\cr whether log2 values are returned, otherwise original scale is used.
#' @param lib_size library size, defaults to colSums(y)
#' @param prior_count average count to be added to each observation to avoid 
#'   taking log of zero, used only when log=TRUE
#'   
#' @return List with the above settings used to perform the normalization procedure.
#' 
#' @note Note, to be used with the normalize function.
#'   
#' @export
#' @examples
#' control <- control_normalize()
#' control1 <- control_normalize(log = FALSE, lib_size = 1000000L, prior_count = 0.5)
#'               
control_normalize <- function(log = TRUE,
                              lib_size = NULL,
                              prior_count = 1) {
  assert_that(
    is.flag(log),
    is.null(lib_size) || utils.nest::is_integer_vector(lib_size),
    is.number(prior_count) && prior_count >= 0
  )
  list(
    log = log,
    lib_size = lib_size,
    prior_count = prior_count
  )
}
