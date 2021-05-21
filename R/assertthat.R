#' Failure message for is() function to get class assertions
#'
#'This function adds a failure message when class assertions 
#'checked by is() are not met
#'
#' @note Note this inherits from
#'      is()
#' 
#' @name on_failure
#' @param  
#' @return
#' @importMethodsFrom methods 'is<-'
#' @returns Failure message when class assertion not met 
#'
#' @examples
#' is()


#questions? We don't export this function, correct?

on_failure(is) <- function(call, env) {
  obj_name <- deparse(call$object)
  class <- eval(call$class2, env)
  paste(obj_name, "is not of class", class)
}

library(rnaseqTools)