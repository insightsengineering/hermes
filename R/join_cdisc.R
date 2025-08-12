#' Sample Variables with Selected Gene Information
#'
#' @description
#'
#' This obtains the sample variables of a `HermesData` object together
#' with selected gene information.
#'
#' @param object (`AnyHermesData`)\cr input experiment.
#' @param assay_name (`string`)\cr which assay to use.
#' @param genes (`GeneSpec`)\cr which genes or which gene signature should be extracted.
#'
#' @return The combined data set, where the additional attribute `gene_cols` contains
#'   the names of the columns obtained by extracting the `genes` information.
#'
#' @note The class of the returned data set will depend on the class of `colData`, so usually
#'   will be [`S4Vectors::DFrame`].
#' @export
#'
#' @examples
#' result <- col_data_with_genes(hermes_data, "counts", gene_spec("GeneID:1820"))
#' tail(names(result))
#' result$GeneID.1820
col_data_with_genes <- function(object,
                                assay_name,
                                genes) {
  assert_class(object, "AnyHermesData")
  assert_string(assay_name)
  assert_class(genes, "GeneSpec")

  col_data <- colData(object)
  assay_matrix <- assay(object, assay_name)
  gene_data <- genes$extract_data_frame(assay_matrix)

  assert_true(identical(
    rownames(col_data),
    rownames(gene_data)
  ))
  structure(
    cbind(
      col_data,
      gene_data
    ),
    gene_cols = names(gene_data)
  )
}

#' Inner Joining a Genes with a CDISC Data Set
#'
#' @description
#'
#' This is a useful function when trying to join genetic with CDISC data sets.
#'
#' @param gene_data (`data.frame` or `DataFrame`)\cr genetic data.
#' @param cdisc_data (`data.frame`)\cr CDISC data (typically patient level data).
#' @param patient_key (`string`)\cr patient identifier.
#' @param additional_keys (`character`)\cr potential additional keys for the two data sets.
#'
#' @return A `data.frame` which contains columns from both data sets merged by the keys.
#'
#' @note Columns which are contained in both data sets but are not specified as keys are taken
#'   from `gene_data` and not from `cdisc_data`.
#' @export
#'
#' @examples
#' gene_data <- col_data_with_genes(hermes_data, "counts", gene_spec("GeneID:1820"))
#' cdisc_data <- data.frame(
#'   USUBJID = head(gene_data$USUBJID, 10),
#'   extra = 1:10
#' )
#' result <- inner_join_cdisc(gene_data, cdisc_data)
#' result
inner_join_cdisc <- function(gene_data,
                             cdisc_data,
                             patient_key = "USUBJID",
                             additional_keys = character()) {
  assert_subset(class(gene_data), choices = c("data.frame", "DFrame", "DataFrame"))
  assert_data_frame(cdisc_data)
  assert_string(patient_key, min.chars = 1L)
  assert_character(additional_keys, min.chars = 1L, any.missing = FALSE, unique = TRUE)

  all_keys <- union(patient_key, additional_keys)
  assert_names(names(gene_data), must.include = all_keys)
  assert_names(names(cdisc_data), must.include = all_keys)

  # Inner join by provided keys.
  cols_to_take_from_gene_data <- setdiff(names(gene_data), all_keys)
  cols_from_cdisc_data <- setdiff(names(cdisc_data), cols_to_take_from_gene_data)
  cdisc_data <- cdisc_data[, cols_from_cdisc_data, drop = FALSE]
  result <- merge(gene_data, cdisc_data, by = all_keys)

  # Check whether patients were lost from `gene_data`.
  gene_patients <- unique(gene_data[[patient_key]])
  result_patients <- unique(result[[patient_key]])
  patients_not_in_cdisc <- setdiff(gene_patients, result_patients)
  if (length(patients_not_in_cdisc) > 0) {
    warning(
      "Patients ", toString(patients_not_in_cdisc),
      " from gene data set were lost because they could not be joined to CDISC data set"
    )
  }

  as.data.frame(result)
}
