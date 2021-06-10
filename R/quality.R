#' Control for Specified Quality Flags
#' 
#' Control function which specifies the quality flag settings.
#' One or more settings can be customized. Not specified settings are left at defaults.
#' 
#' @param min_cpm (non-negative `number`)\cr minimum CPM for each gene within
#'   the sample.
#' @param min_cpm_prop (`proportion`)\cr minimum percentage of samples with
#'   acceptable CPM of certain gene for low expression flagging.
#' @param min_corr (`proportion`)\cr minimum Pearson correlation coefficient of
#'   CPM between samples for technical failure flagging.
#' @param min_depth (non-negative `count` or `NULL`)\cr minimum library depth
#'   for low depth flagging. If `NULL`, this will be calculated as the first
#'   quartile minus 1.5 times the inter-quartile range of the library size
#'   (depth) of all samples. (So anything below the usual lower boxplot whisker
#'   would be too low.)
#'   
#' @return List with the above criteria to flag observations.
#' 
#' @note To be used with the [add_quality_flags()] function.
#'
#' @importFrom tern is_proportion
#' @importFrom tern is_nonnegative_count
#' @export
#' 
#' @examples
#' # Default settings.
#' control_quality()
#' 
#' # One or more settings can be customized.
#' control_quality(min_cpm = 5, min_cpm_prop = 0.001)
#'               
control_quality <- function(min_cpm = 1,
                            min_cpm_prop = 0.25,
                            min_corr = 0.5,
                            min_depth = NULL) {
  assert_that(
    is.number(min_cpm) && min_cpm >= 0,
    tern::is_proportion(min_cpm_prop),
    tern::is_proportion(min_corr),
    is.null(min_depth) || tern::is_nonnegative_count(min_depth)
  )  
  list(
    min_cpm = min_cpm,
    min_cpm_prop = min_cpm_prop,
    min_corr = min_corr,
    min_depth = min_depth
  )
}

#' Add Quality Flags
#' 
#' This function adds quality flag information to a [AnyHermesData] object:
#' - `LowExpressionFlag`: for each gene, counts how many samples don't pass a minimum 
#'   expression CPM threshold. If too many, then it flags this gene as "low expression" gene.
#' - `TechnicalFailureFlag`: first calculates the Pearson correlation matrix of the sample wise
#'   CPM values, resulting in a matrix measuring the correlation between samples. 
#'   Then compares the average correlation per sample with a threshold - if it is too low, 
#'   then the sample is flagged as "technical failure".
#' - `LowDepthFlag`: computes the library size (total number of counts) per sample 
#'   (removing any NAs). If this number is too low, the sample is flagged as "low depth".
#' 
#' While `object` already has the variables above (as this is enforced by the validation method
#' for [AnyHermesData]), they are usually still `NA` after the initial creation.
#' 
#' @param object (`AnyHermesData`) \cr input.
#' @param control (`list`) \cr list of settings used to perform the quality control procedure, 
#'   produced by [control_quality()].
#' @param overwrite (`flag`)\cr whether previous results need to be overwritten.
#'   
#' @return The input object with added quality flags.
#' 
#' @seealso [control_quality()] for the detailed settings specifications.
#' 
#' @export
#' 
#' @examples
#' # Adding default quality flags to HermesData object.
#' object <- HermesData(summarized_experiment)
#' result <- add_quality_flags(object)
#' head(rowData(result)$LowExpressionFlag)
#' head(colData(result)$TechnicalFailureFlag)
#' head(colData(result)$LowDepthFlag)
#' 
#' # It is possible to overwrite flags if needed, which will trigger a message.
#' result2 <- add_quality_flags(result, control_quality(min_cpm = 1000), overwrite = TRUE)
#'               
add_quality_flags <- function(object, 
                              control = control_quality(),
                              overwrite = FALSE) {
  assert_that(
    is_hermes_data(object),
    is.flag(overwrite)
  )
  already_added <- ("control_quality_flags" %in% names(metadata(object))) 
  if (already_added) {
    if (overwrite) {
      message("previously have added quality flags, but overwriting now")
    } else {
      stop("previously have added quality flags, please double check or ask for overwrite")
    }
  }
  
  rowData(object)$LowExpressionFlag <- h_low_expression_flag(object, control)
  colData(object)$TechnicalFailureFlag <- h_tech_failure_flag(object, control)
  colData(object)$LowDepthFlag <- h_low_depth_flag(object, control)
  
  metadata(object)$control_quality_flags <- control
  
  object
}

#' @describeIn add_quality_flags creates the low expression flag for genes 
#'   given control settings.
#' 
#' @importFrom edgeR cpm
#' @export
#' 
#' @examples
#' # Separate calculation of low expression flag.
#' low_expr_flag <- h_low_expression_flag(object)
#' head(low_expr_flag)
#' length(low_expr_flag) == nrow(object)
#' 
#' low_expr_flag2 <- h_low_expression_flag(
#'   object, 
#'   control_quality(min_cpm = 500, min_cpm_prop = 0.9)
#' )
#' head(low_expr_flag2)
#' 
h_low_expression_flag <- function(object, 
                                  control = control_quality()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, c("min_cpm_prop", "min_cpm"))
  )
  cpm <- edgeR::cpm(counts(object))
  threshold_n_samples <- ceiling(ncol(cpm) * control$min_cpm_prop)
  n_samples_below_min_cpm <- rowSums(cpm <= control$min_cpm)
  n_samples_below_min_cpm > threshold_n_samples
}

#' @describeIn add_quality_flags creates the low depth (library size) flag for samples 
#'   given control settings.
#' 
#' @importFrom stats quantile
#' @export
#' 
#' @examples
#' low_depth_flag <- h_low_depth_flag(object)
#' head(low_depth_flag)
#' length(low_depth_flag) == ncol(object)
#' 
#' low_depth_flag2 <- h_low_depth_flag(object, control_quality(min_depth = 5))
#' head(low_depth_flag2)
#' 
h_low_depth_flag <- function(object,
                             control = control_quality()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, "min_depth")
  )
  lib_sizes <- colSums(counts(object))
  if (is.null(control$min_depth)) {
    lower_upper_quartiles <- quantile(lib_sizes, probs = c(0.25, 0.75))
    control$min_depth <- lower_upper_quartiles[1] - 1.5 * diff(lower_upper_quartiles)
  }
  lib_sizes < control$min_depth
}

#' @describeIn add_quality_flags creates the technical failure flag for samples 
#'   given control settings.
#'   
#' @importFrom edgeR cpm
#' @export
#'
#' @examples
#' object <- HermesData(summarized_experiment)
#' tech_failure_flag <- h_tech_failure_flag(object)
#' head(tech_failure_flag)
#' length(tech_failure_flag) == ncol(object)
#' 
#' tech_failure_flag2 <- h_tech_failure_flag(object, control_quality(min_corr = 0.35))
#' head(tech_failure_flag2)
#' 
h_tech_failure_flag <- function(object,
                                control = control_quality()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, "min_corr")
  )
  cpm <- edgeR::cpm(counts(object))
  corr_matrix <- stats::cor(cpm, method = "pearson")
  colMeans(corr_matrix) < control$min_corr
}

#' Flag Technical Failures
#' 
#' Setter function which allows the user to define a sample manually as a technical failure. 
#' 
#' @param object (`AnyHermesData`)\cr input.
#' @param sample_ids (`list`) \cr list of sample IDs to be flagged manually as technical failures.
#'   
#' @return HermesData object with modified technical failure flags.
#' 
#' @export
#' 
#' @examples
#' # Manually flag technical failures in a HermesData object.
#' object <- HermesData(summarized_experiment)
#' result <- flag_tech_failure(object, c("06520101B0017R", "06520047C0017R"))
#' 
flag_tech_failure <- function(object,
                              sample_ids){
  assert_that(
    is_hermes_data(object),
    is_character_vector(sample_ids)
  )
  samples <- colnames(object)
  matches <- match(sample_ids, samples)
  colData(object)$TechnicalFailureFlag[matches] <- TRUE
  
  object
}