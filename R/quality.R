#' Control for Specified Quality Flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Control function which specifies the quality flag settings.
#' One or more settings can be customized. Not specified settings are left at defaults.
#'
#' @param min_cpm (non-negative `number`)\cr minimum Counts per Million (`CPM`) for
#'   each gene within the sample.
#' @param min_cpm_prop (`proportion`)\cr minimum proportion of samples with
#'   acceptable `CPM` of certain gene for low expression flagging.
#' @param min_corr (`proportion`)\cr minimum Pearson correlation coefficient of
#'   `CPM` between samples for technical failure flagging.
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
#' @export
#'
#' @examples
#' # Default settings.
#' control_quality()
#'
#' # One or more settings can be customized.
#' control_quality(min_cpm = 5, min_cpm_prop = 0.001)
control_quality <- function(min_cpm = 1,
                            min_cpm_prop = 0.25,
                            min_corr = 0.5,
                            min_depth = NULL) {
  assert_that(
    is.number(min_cpm) && min_cpm >= 0
  )
  expect_proportion(min_cpm_prop)
  expect_proportion(min_corr)
  expect_count(min_depth, positive = TRUE, null.ok = TRUE)

  list(
    min_cpm = min_cpm,
    min_cpm_prop = min_cpm_prop,
    min_corr = min_corr,
    min_depth = min_depth
  )
}

#' Add Quality Flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The function `add_quality_flags()` adds quality flag information to a [`AnyHermesData`] object:
#' - `low_expression_flag`: for each gene, counts how many samples don't pass a minimum
#'   expression Counts per Million (`CPM`) threshold. If too many, then it flags this gene
#'   as a "low expression" gene.
#' - `tech_failure_flag`: first calculates the Pearson correlation matrix of the sample wise
#'   `CPM` values, resulting in a matrix measuring the correlation between samples.
#'   Then compares the average correlation per sample with a threshold - if it is too low,
#'   then the sample is flagged as a "technical failure" sample.
#' - `low_depth_flag`: computes the library size (total number of counts) per sample.
#'   If this number is too low, the sample is flagged as a "low depth" sample.
#'
#' Separate helper functions are internally used to create the flags, and
#' separate `getter` functions allow easy access to the quality control flags in an object.
#'
#' @rdname quality_flags
#'
#' @details While `object` already has the variables mentioned above as part of the
#'   `rowData` and `colData` (as this is enforced by the validation
#'   method for [`AnyHermesData`]), they are usually still `NA` after the initial
#'   object creation.
#'
#' @param object (`AnyHermesData`) \cr input.
#' @param control (`list`) \cr list of settings (thresholds etc.) used to compute the
#'   quality control flags, produced by [control_quality()].
#' @param overwrite (`flag`)\cr whether previously added flags may be overwritten.
#'
#' @return The input object with added quality flags.
#'
#' @seealso
#'   - [control_quality()] for the detailed settings specifications;
#'   - [set_tech_failure()] to manually flag samples as technical failures.
#'
#' @export
#'
#' @examples
#' # Adding default quality flags to `AnyHermesData` object.
#' object <- hermes_data
#' result <- add_quality_flags(object)
#' which(get_tech_failure(result) != get_tech_failure(object))
#' head(get_low_expression(result))
#' head(get_tech_failure(result))
#' head(get_low_depth(result))
#'
#' # It is possible to overwrite flags if needed, which will trigger a message.
#' result2 <- add_quality_flags(result, control_quality(min_cpm = 1000), overwrite = TRUE)
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

  rowData(object)$low_expression_flag <- h_low_expression_flag(object, control) # nolint
  colData(object)$tech_failure_flag <- h_tech_failure_flag(object, control) # nolint
  colData(object)$low_depth_flag <- h_low_depth_flag(object, control) # nolint

  metadata(object)$control_quality_flags <- control

  object
}

#' @describeIn quality_flags creates the low expression flag for genes
#'   given control settings.
#'
#' @export
#'
#' @examples
#'
#' # Separate calculation of low expression flag.
#' low_expr_flag <- h_low_expression_flag(
#'   object,
#'   control_quality(min_cpm = 500, min_cpm_prop = 0.9)
#' )
#' length(low_expr_flag) == nrow(object)
#' head(low_expr_flag)
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

#' @describeIn quality_flags creates the low depth (library size) flag for samples
#'   given control settings.
#'
#' @export
#'
#' @examples
#'
#' # Separate calculation of low depth flag.
#' low_depth_flag <- h_low_depth_flag(object, control_quality(min_depth = 5))
#' length(low_depth_flag) == ncol(object)
#' head(low_depth_flag)
h_low_depth_flag <- function(object,
                             control = control_quality()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, "min_depth")
  )
  lib_sizes <- colSums(counts(object))
  if (is.null(control$min_depth)) {
    lower_upper_quartiles <- stats::quantile(lib_sizes, probs = c(0.25, 0.75))
    control$min_depth <- lower_upper_quartiles[1] - 1.5 * diff(lower_upper_quartiles)
  }
  lib_sizes < control$min_depth
}

#' @describeIn quality_flags creates the technical failure flag for samples
#'   given control settings.
#'
#' @export
#'
#' @examples
#'
#' # Separate calculation of technical failure flag.
#' tech_failure_flag <- h_tech_failure_flag(object, control_quality(min_corr = 0.35))
#' length(tech_failure_flag) == ncol(object)
#' head(tech_failure_flag)
h_tech_failure_flag <- function(object,
                                control = control_quality()) {
  assert_that(
    is_hermes_data(object),
    is_list_with(control, "min_corr")
  )
  cpm <- edgeR::cpm(counts(object))

  if (nrow(cpm) > 1) {
    corr_matrix <- stats::cor(cpm, method = "pearson")
    colMeans(corr_matrix) < control$min_corr
  } else {
    stats::setNames(
      rep(FALSE, ncol(cpm)),
      colnames(cpm)
    )
  }
}

#' @describeIn quality_flags get the technical failure flags for all samples.
#' @export
#' @examples
#' head(get_tech_failure(object))
get_tech_failure <- function(object) {
  assert_that(is_hermes_data(object))
  flag_vals <- colData(object)$tech_failure_flag
  samples <- colnames(object)
  stats::setNames(flag_vals, samples)
}

#' @describeIn quality_flags get the low depth failure flags for all samples.
#' @export
#' @examples
#' head(get_low_depth(object))
get_low_depth <- function(object) {
  assert_that(is_hermes_data(object))
  flag_vals <- colData(object)$low_depth_flag
  samples <- colnames(object)
  stats::setNames(flag_vals, samples)
}

#' @describeIn quality_flags get the low expression failure flags for all genes.
#' @export
#' @examples
#' head(get_low_expression(object))
get_low_expression <- function(object) {
  assert_that(is_hermes_data(object))
  flag_vals <- rowData(object)$low_expression_flag
  genes <- rownames(object)
  stats::setNames(flag_vals, genes)
}

#' Set Technical Failure Flags
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Setter function which allows the user to define a sample manually as a technical failure.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param sample_ids (`character`) \cr sample IDs to be flagged as technical failures.
#'
#' @return [`AnyHermesData`] object with modified technical failure flags.
#' @seealso [add_quality_flags()] which automatically sets all (gene and sample) quality flags,
#'   including these technical failure flags.
#'
#' @export
#'
#' @examples
#' # Manually flag technical failures in a `AnyHermesData` object.
#' object <- hermes_data
#' get_tech_failure(object)["06520101B0017R"]
#' result <- set_tech_failure(object, c("06520101B0017R", "06520047C0017R"))
#' get_tech_failure(result)["06520101B0017R"]
set_tech_failure <- function(object,
                             sample_ids) {
  assert_character(sample_ids, any.missing = FALSE)
  assert_that(
    is_hermes_data(object),
    all(sample_ids %in% colnames(object))
  )
  matches <- match(sample_ids, colnames(object))
  colData(object)$tech_failure_flag[matches] <- TRUE
  object
}
