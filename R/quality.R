#' Quality Control - Low Expression Flag
#'
#' @param object (`HermesData`) \cr input.
#' @param control (`list`) \cr list of settings used to perform the normalization procedure.
#'
#' @return A logical vector indicating whether each gene in HermesData object has low expression
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' res <- h_low_expression_flag(object, cont)
#' head(res)
#' 

## Adding control_quality part to be able to program h_low_expression_flag()
## !!!!!! REMOVE this part once control_quality function is added !!!!!!
control_quality <- function(expression_min_cpm = 1,
                            expression_min_prop = 0.25,
                            technical_min_corr = 0.5,
                            depth_min = NULL) {
  assert_that(
    is.number(expression_min_cpm) && expression_min_cpm >= 0,
    tern::is_proportion(expression_min_prop),
    tern::is_proportion(technical_min_corr),
    is.null(depth_min) || is.number(depth_min)
  )
  list(
    expression_min_cpm = expression_min_cpm,
    expression_min_prop = expression_min_prop,
    technical_min_corr = technical_min_corr,
    depth_min = depth_min
  )
}
cont <- control_quality()


h_low_expression_flag <- function(object, control) {
  assert_that(
    is_hermes_data(object),
    utils.nest::is_fully_named_list(control)
  )
  cpm <- edgeR::cpm(counts(object))
  threshold_n_samples <- ceiling(ncol(cpm) * control$expression_min_prop)
  n_samples_below_min_cpm <- rowSums(cpm <= control$expression_min_cpm)
  n_samples_below_min_cpm > threshold_n_samples
}