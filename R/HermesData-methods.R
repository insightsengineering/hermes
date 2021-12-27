# rbind ----

#' Row Binding of `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This method combines [`AnyHermesData`] objects with the same samples but different
#' features of interest (rows in assays).
#'
#' @note
#'   - Note that this just inherits
#'     [SummarizedExperiment::rbind,SummarizedExperiment-method()]. When binding a
#'     [`AnyHermesData`] object with a [`SummarizedExperiment::SummarizedExperiment`]
#'     object, then the result will be a
#'     [`SummarizedExperiment::SummarizedExperiment`] object (the more general
#'     class).
#'   - Note that we need to have unique gene IDs (row names) and the same prefix
#'     across the combined object.
#'
#' @name rbind
#'
#' @param ... (`AnyHermesData`)\cr objects to row bind.
#'
#' @return The combined [`AnyHermesData`] object.
#'
#' @seealso [`cbind`] to column bind objects.
#'
#' @examples
#' a <- hermes_data[1:2542, ]
#' b <- hermes_data[2543:5085, ]
#' result <- rbind(a, b)
#' class(result)
NULL

# cbind ----

#' Column Binding of `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This method combines [`AnyHermesData`] objects with the same ranges but different
#' samples (columns in assays).
#'
#' @note
#'   - Note that this just inherits
#'     [SummarizedExperiment::cbind,SummarizedExperiment-method()]. When binding a
#'     [`AnyHermesData`] object with a [`SummarizedExperiment::SummarizedExperiment`]
#'     object, then the result will be a
#'     [`SummarizedExperiment::SummarizedExperiment`] object (the more general
#'     class).
#'   - Note that the combined object needs to have unique sample IDs (column names).
#'
#' @name cbind
#'
#' @param ... (`AnyHermesData`)\cr objects to column bind.
#'
#' @return The combined [`AnyHermesData`] object.
#'
#' @seealso [`rbind`] to row bind objects.
#'
#' @examples
#' a <- hermes_data[, 1:10]
#' b <- hermes_data[, 11:20]
#' result <- cbind(a, b)
#' class(result)
NULL

# metadata ----

#' Metadata Accessor and Setter
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These methods access or set the metadata in a [`AnyHermesData`] object.
#'
#' @note Note that this just inherits [S4Vectors::metadata,Annotated-method()].
#'
#' @name metadata
#'
#' @param x (`AnyHermesData`)\cr object to access the metadata from.
#' @param value (`list`)\cr the list to replace the current metadata with.
#'
#' @return The metadata which is a list.
#' @importFrom S4Vectors `metadata<-`
#' @importMethodsFrom S4Vectors metadata
#' @exportMethod metadata
#' @export `metadata<-`
#'
#' @examples
#' a <- hermes_data
#' metadata(a)
#' metadata(a) <- list(new = "my metadata")
#' metadata(a)
NULL

# annotation ----

#' Annotation Accessor and Setter
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These methods access and set the gene annotations stored in a [`AnyHermesData`] object.
#'
#' @rdname annotation
#' @aliases annotation
#'
#' @param object (`AnyHermesData`)\cr object to access the annotations from.
#' @param ... not used.
#'
#' @return The [`S4Vectors::DataFrame`] with the gene annotations:
#'   - `symbol`
#'   - `desc`
#'   - `chromosome`
#'   - `size`
#'
#' @importFrom BiocGenerics annotation
#' @export
#'
#' @examples
#' object <- hermes_data
#' head(annotation(object))
setMethod(
  f = "annotation",
  signature = c(object = "AnyHermesData"),
  definition = function(object, ...) {
    rowData(object)[, .row_data_annotation_cols]
  }
)

#' @rdname annotation
#' @format The annotation column names are available in the exported
#'   character vector `.row_data_annotation_cols`.
#' @export
.row_data_annotation_cols <- c(
  "symbol",
  "desc",
  "chromosome",
  "size"
)

#' @param value (`DataFrame`)\cr what should the annotations be replaced with.
#'
#' @note When trying to replace the required annotations with completely missing
#'   values for any genes, a warning will be given and the corresponding gene
#'   IDs will be saved in the attribute `annotation.missing.genes`. Note also
#'   that additional annotations beyond the required ones may be supplied and
#'   will be stored.
#'
#' @importFrom BiocGenerics `annotation<-`
#' @rdname annotation
#' @export
setReplaceMethod(
  f = "annotation",
  signature = c(object = "AnyHermesData", value = "DataFrame"),
  definition = function(object, value) {
    assert_that(
      identical(rownames(object), rownames(value)),
      all(.row_data_annotation_cols %in% colnames(value))
    )
    row_is_all_na <- apply(X = value[, .row_data_annotation_cols], MARGIN = 1L, FUN = all_na)
    if (any(row_is_all_na)) {
      warning(
        "required annotations completely missing for ", sum(row_is_all_na), " genes, ",
        "see attribute `annotation.missing.genes` for the corresponding gene IDs"
      )
      attr(object, "annotation.missing.genes") <- names(which(row_is_all_na))
    }
    rowData(object)[, .row_data_annotation_cols] <- value[, .row_data_annotation_cols]
    add_annotation_cols <- setdiff(colnames(value), .row_data_annotation_cols)
    if (length(add_annotation_cols)) {
      rowData(object)[, add_annotation_cols] <- value[, add_annotation_cols, drop = FALSE]
    }
    validObject(object)
    object
  }
)

# counts ----

#' Counts Accessor and Setter
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These methods access and set the counts assay in a [`AnyHermesData`] object.
#'
#' @rdname counts
#' @aliases counts
#'
#' @param object (`AnyHermesData`)\cr object to access the counts from.
#' @param value (`matrix`)\cr what should the counts assay be replaced with.
#'
#' @return The counts assay.
#'
#' @importFrom BiocGenerics counts
#' @export
#'
#' @examples
#' a <- hermes_data
#' result <- counts(a)
#' class(result)
#' head(result)
setMethod(
  f = "counts",
  signature = "AnyHermesData",
  definition = function(object) {
    assay(object)
  }
)

#' @describeIn counts
#'
#' @param withDimnames (`flag`)\cr setting `withDimnames =FALSE` in the setter
#'   (`counts<-`) is required when the `dimnames` on the supplied counts assay
#'   are not identical to the `dimnames` on the `AnyHermesData` object;
#'   it does not influence actual assignment of `dimnames` to the assay
#'   (they're always stored as-is).
#' @importFrom BiocGenerics `counts<-`
#' @export
#'
#' @examples
#' counts(a) <- counts(a) + 100L
#' head(counts(a))
setReplaceMethod(
  f = "counts",
  signature = signature(object = "AnyHermesData", value = "matrix"),
  definition = function(object, value, withDimnames = TRUE) {
    assay(object, withDimnames = withDimnames) <- value
    validObject(object)
    object
  }
)

# prefix ----

#' Prefix Accessor
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Generic function to access the prefix from an object.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param ... additional arguments.
#'
#' @return The `prefix` slot contents.
#' @export
#'
#' @examples
#' a <- hermes_data
#' prefix(a)
setGeneric("prefix", def = function(object, ...) {
  object@prefix
})

# genes ----

#' Gene IDs Accessor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Access the gene IDs, i.e. row names, of a [`AnyHermesData`] object with a
#' nicely named accessor method.
#'
#' @param object (`AnyHermesData`)\cr input.
#'
#' @return The character vector with the gene IDs.
#'
#' @seealso [samples()] to access the sample IDs.
#'
#' @export
setGeneric("genes", def = function(object) standardGeneric("genes"))

#' @rdname genes
#' @export
#' @examples
#' a <- hermes_data
#' genes(a)
setMethod(
  f = "genes",
  signature = c(object = "AnyHermesData"),
  definition = function(object) {
    rownames(object)
  }
)

# samples ----

#' Sample IDs Accessor
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Access the sample IDs, i.e. col names, of a [`AnyHermesData`] object with a
#' nicely named accessor method.
#'
#' @param object (`AnyHermesData`)\cr input.
#'
#' @return The character vector with the sample IDs.
#'
#' @rdname samples
#' @aliases samples
#'
#' @seealso [genes()] to access the gene IDs.
#'
#' @importFrom Biobase samples
#' @export
#' @examples
#' a <- hermes_data
#' samples(a)
setMethod(
  f = "samples",
  signature = c(object = "AnyHermesData"),
  definition = function(object) {
    colnames(object)
  }
)

# subset ----

#' Subsetting `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This method subsets [`AnyHermesData`] objects, based on expressions involving the
#' `rowData` columns and the `colData` columns.
#'
#' @note Note that this just inherits
#'   [SummarizedExperiment::subset,SummarizedExperiment-method()].
#'
#' @name subset
#'
#' @param x (`AnyHermesData`)\cr object to subset from.
#' @param subset (`expression`)\cr logical expression based on the `rowData` columns to
#'   select genes.
#' @param select (`expression`)\cr logical expression based on the `colData` columns to
#'   select samples.
#'
#' @return The subsetted [`AnyHermesData`] object.
#'
#' @examples
#' a <- hermes_data
#' a
#'
#' # Subset both genes and samples.
#' subset(a, subset = low_expression_flag, select = DISCSTUD == "N")
#'
#' # Subset only genes.
#' subset(a, subset = chromosome == "2")
#'
#' # Subset only samples.
#' subset(a, select = AGE > 18)
NULL

# filter ----

#' Filter `AnyHermesData` on Subset Passing Default QC Flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This filters a [`AnyHermesData`] object using the default QC flags and required annotations.
#'
#' @param object (`AnyHermesData`)\cr object to filter.
#' @param ... additional arguments.
#' @return The filtered [`AnyHermesData`] object.
#'
#' @export
setGeneric("filter", function(object, ...) standardGeneric("filter"))

#' Predicate for Required Annotations
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function determines for each gene in the object whether all required
#' annotation columns are filled.
#'
#' @param object (`AnyHermesData`)\cr input object.
#' @param annotation_required (`character`)\cr names of required [`annotation`] columns for genes.
#'
#' @return Named logical vector with one value for each gene in `object`, which is `TRUE` if all
#'   required annotation columns are filled, and otherwise `FALSE`.
#'
#' @seealso [filter()] where this is used internally.
#' @export
#'
#' @examples
#' object <- hermes_data
#' result <- h_has_req_annotations(object, "size")
#' all(result)
#' rowData(object)$size[1] <- NA # nolint
#' which(!h_has_req_annotations(object, "size"))
h_has_req_annotations <- function(object,
                                  annotation_required) {
  assert_that(
    is_hermes_data(object),
    all(annotation_required %in% .row_data_annotation_cols)
  )
  annotation_req_cols <- annotation(object)[, annotation_required, drop = FALSE]
  apply(X = annotation_req_cols, MARGIN = 1L, FUN = noNA)
}

#' @rdname filter
#'
#' @details
#' - Only genes without low expression (`low_expression_flag`) and samples
#'   without low depth (`low_depth_flag`) or technical failure (`tech_failure_flag`)
#'   remain in the returned filtered object.
#' - Also required gene annotation columns can be specified, so that genes which are not complete
#'   for these columns are filtered out. By default this is the `size` column, which is needed
#'   for default normalization of the object.
#'
#' @param what (`character`)\cr specify whether to apply the filter on `genes` and / or `samples`.
#' @param annotation_required (`character`)\cr names of required [`annotation`] columns for genes. Only
#'   used when `genes` are filtered.
#'
#' @note The internal implementation cannot use the [subset()] method since that
#'   requires non-standard evaluation of arguments.
#'
#' @export
#' @examples
#' a <- hermes_data
#' dim(a)
#'
#' # Filter genes and samples on default QC flags.
#' result <- filter(a)
#' dim(result)
#'
#' # Filter only genes without low expression.
#' result <- filter(a, what = "genes")
#'
#' # Filter only samples with low depth and technical failure.
#' result <- filter(a, what = "samples")
#'
#' # Filter only genes, and require certain annotations to be present.
#' result <- filter(a, what = "genes", annotation_required = c("size"))
setMethod(
  f = "filter",
  signature = signature(object = "AnyHermesData"),
  definition = function(object,
                        what = c("genes", "samples"),
                        annotation_required = "size") {
    low_exp <- get_low_expression(object)
    low_depth <- get_low_depth(object)
    tech_fail <- get_tech_failure(object)
    what <- match.arg(what, c("genes", "samples"), several.ok = TRUE)
    assert_that(
      noNA(low_exp),
      noNA(low_depth),
      noNA(tech_fail),
      msg = "still NA in quality flags, please first run add_quality_flags() to fill them"
    )
    rows <- if ("genes" %in% what) {
      !low_exp & h_has_req_annotations(object, annotation_required)
    } else {
      rep_len(TRUE, length(low_exp))
    }
    cols <- if ("samples" %in% what) {
      !low_depth & !tech_fail
    } else {
      rep_len(TRUE, length(low_depth))
    }
    if (!any(rows)) {
      warning("filtering out all genes")
    }
    if (!any(cols)) {
      warning("filtering out all samples")
    }
    object[rows, cols]
  }
)

#' Extra Variable Names Accessor Methods
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The methods access the names of the variables in `colData()` and `rowData()` of
#' the object which are not required by design. So these can be additional sample or
#' patient characteristics, or gene characteristics.
#'
#' @name extra_data_names
#'
#' @param x (`AnyHermesData`)\cr object.
#' @param ... not used.
#'
#' @return The character vector with the additional variable names in either
#'   `colData()` or `rowData()`.
#'
#' @examples
#' object <- hermes_data
NULL

# extraColDataNames ----

#' @rdname extra_data_names
#' @export
setGeneric("extraColDataNames", function(x, ...) standardGeneric("extraColDataNames"))

#' @rdname extra_data_names
#' @export
#' @examples
#' extraColDataNames(object)
setMethod(
  f = "extraColDataNames",
  signature = c(x = "AnyHermesData"),
  definition = function(x, ...) {
    cd_names <- colnames(colData(x))
    cd_req_names <- .col_data_cols
    setdiff(cd_names, cd_req_names)
  }
)

# extraRowDataNames ----

#' @rdname extra_data_names
#' @export
setGeneric("extraRowDataNames", function(x, ...) standardGeneric("extraRowDataNames"))

#' @rdname extra_data_names
#' @export
#' @examples
#' extraRowDataNames(object)
setMethod(
  f = "extraRowDataNames",
  signature = c(x = "AnyHermesData"),
  definition = function(x, ...) {
    rd_names <- colnames(rowData(x))
    rd_req_names <- .row_data_cols
    setdiff(rd_names, rd_req_names)
  }
)

# rename ----

#' Helper Function For Matching Map Values to Names
#'
#' This is used by the [`rename`] method. It wraps the assertions and the
#' matching used several times.
#'
#' @param names (`character`)\cr original names.
#' @param map (named `character`)\cr the mapping vector from old (value) to new
#'   (name) names. All values must be included in `names`.
#'
#' @return Integer vector of the positions of the `map` values in the `names`.
#' @export
#'
#' @examples
#' h_map_pos(c("a", "b"), c(d = "b"))
h_map_pos <- function(names, map) {
  assert_character(
    names,
    any.missing = FALSE,
    unique = TRUE
  )
  assert_character(
    map,
    min.chars = 1L,
    any.missing = FALSE,
    unique = TRUE,
    names = "unique"
  )
  assert_subset(map, names)
  match(map, names)
}

#' Renaming Contents of `SummarizedExperiment` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This method renames columns of the `rowData` and `colData`, as well as assays, of
#' [`SummarizedExperiment::SummarizedExperiment`] objects. This increases the flexibility
#' since renaming can be done before conversion to a [`HermesData`] object.
#'
#' @rdname rename
#' @aliases rename
#'
#' @param x (`SummarizedExperiment`)\cr object to rename contents in.
#' @param row_data (named `character`)\cr mapping from existing (right-hand side values)
#'   to new (left-hand side names) column names of `rowData`.
#' @param col_data (named `character`)\cr mapping from existing (right-hand side values)
#'   to new (left-hand side names) column names of `colData`.
#' @param assays (named `character`)\cr mapping from existing (right-hand side values)
#'   to new (left-hand side names) assay names.
#' @param ... additional arguments (not used here).
#'
#' @return The [`SummarizedExperiment::SummarizedExperiment`] object with renamed contents.
#'
#' @importFrom S4Vectors `rename`
#' @export
#' @examples
#' x <- summarized_experiment
#'
#' # Rename `HGNC` to `symbol` in the `rowData`.
#' x <- rename(x, row_data = c(symbol = "HGNC"))
#' head(names(rowData(x)))
#'
#' # Rename `LowDepthFlag` to `low_depth_flag` in `colData`.
#' x <- rename(x, col_data = c(low_depth_flag = "LowDepthFlag"))
#' tail(names(colData(x)))
#'
#' # Rename assay `counts` to `count`.
#' x <- rename(x, assays = c(count = "counts"))
#' assayNames(x)
setMethod(
  f = "rename",
  signature = "SummarizedExperiment",
  definition = function(x,
                        row_data = character(),
                        col_data = character(),
                        assays = character(),
                        ...) {
    if (length(row_data)) {
      col_names <- names(rowData(x))
      col_pos <- h_map_pos(names = col_names, map = row_data)
      names(rowData(x))[col_pos] <- names(row_data)
    }
    if (length(col_data)) {
      col_names <- names(colData(x))
      col_pos <- h_map_pos(names = col_names, map = col_data)
      names(colData(x))[col_pos] <- names(col_data)
    }
    if (length(assays)) {
      assay_names <- assayNames(x)
      assay_pos <- h_map_pos(names = assay_names, map = assays)
      assayNames(x)[assay_pos] <- names(assays)
    }
    x
  }
)

# summary ----

#' Summary Method for `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Provides a concise summary of the content of [`AnyHermesData`] objects.
#'
#' @export
setGeneric("summary")

#' @rdname summary
#' @aliases HermesDataSummary
#' @exportClass HermesDataSummary
.HermesDataSummary <- setClass( # nolint
  Class = "HermesDataSummary",
  slots = c(
    class_name = "character",
    n_genes = "integer",
    n_samples = "integer",
    additional_gene_info = "character",
    additional_sample_info = "character",
    no_qc_flags_filled = "logical",
    genes_fail = "character",
    samples_fail = "character",
    lib_sizes = "numeric",
    assay_names = "character"
  )
)

#' @describeIn summary A summary method for [`AnyHermesData`] object that
#'   creates a [`HermesDataSummary`] object.
#'
#' @param object (`AnyHermesData`)\cr input.
#' @param ... not used.
#' @return An object of the corresponding summary class, here
#'   [`HermesDataSummary`].
#'
#' @export
#'
#' @examples
#' object <- hermes_data
#' object_summary <- summary(object)
#'
#' # We can access parts of this S4 object with the `slot` function.
#' str(object_summary)
#' slotNames(object_summary)
#' slot(object_summary, "lib_sizes")
setMethod(
  f = "summary",
  signature = c("AnyHermesData"),
  definition = function(object) {
    low_expression <- get_low_expression(object)
    genes_fail <- rownames(object)[low_expression]

    tech_failure <- get_tech_failure(object)
    low_depth <- get_low_depth(object)
    samples_fail <- colnames(object)[tech_failure | low_depth]

    no_qc_flags_filled <- is.null(metadata(object)$control_quality) &&
      all_na(low_expression) &&
      all_na(tech_failure) &&
      all_na(low_depth)

    .HermesDataSummary(
      class_name = S4Vectors::classNameForDisplay(object),
      n_genes = nrow(object),
      n_samples = ncol(object),
      additional_gene_info = extraRowDataNames(object),
      additional_sample_info = extraColDataNames(object),
      no_qc_flags_filled = no_qc_flags_filled,
      genes_fail = genes_fail,
      samples_fail = samples_fail,
      lib_sizes = colSums(counts(object)),
      assay_names = assayNames(object)
    )
  }
)

#' @describeIn summary A show method prints summary description of [`HermesDataSummary`] object
#'   generated by the [summary()] method.
#'
#' @param object (`HermesDataSummary`) \cr result from the summary method applied to
#'   [`AnyHermesData`] object.
#'
#' @export
#'
#' @examples
#'
#' # Just calling the summary method like this will use the `show()` method.
#' summary(object)
setMethod(
  f = "show",
  signature = c("HermesDataSummary"),
  definition = function(object) {
    cat_with_newline(
      object@class_name, "object with",
      object@n_samples, "samples of", object@n_genes, "genes."
    )
    sum_depth <- summary(object@lib_sizes)
    cat_with_newline(
      "- Library sizes across samples: ",
      "mean ", sum_depth["Mean"], ", median ", sum_depth["Median"], ", range ",
      sum_depth["Min."], " to ", sum_depth["Max."],
      sep = ""
    )
    S4Vectors::coolcat(
      "- Included assays (%d): %s\n",
      object@assay_names
    )
    if (length(object@additional_gene_info)) {
      S4Vectors::coolcat(
        "- Additional gene information (%d): %s\n",
        object@additional_gene_info
      )
    }
    if (length(object@additional_sample_info)) {
      S4Vectors::coolcat(
        "- Additional sample information (%d): %s\n",
        object@additional_sample_info
      )
    }
    if (object@no_qc_flags_filled) {
      cat_with_newline(
        "- QC flags still need to be added"
      )
    } else {
      S4Vectors::coolcat(
        "- Low expression genes (%d): %s\n",
        object@genes_fail
      )
      S4Vectors::coolcat(
        "- Samples with too low depth or technical failures (%d): %s\n",
        object@samples_fail
      )
    }
  }
)

# show ----

.show.AnyHermesData <- function(object) { # nolint
  cat_with_newline(
    "class:",
    S4Vectors::classNameForDisplay(object)
  )
  S4Vectors::coolcat(
    "assays(%d): %s\n",
    assayNames(object)
  )
  S4Vectors::coolcat(
    "genes(%d): %s\n",
    rownames(object)
  )
  S4Vectors::coolcat(
    "additional gene information(%d): %s\n",
    extraRowDataNames(object)
  )
  S4Vectors::coolcat(
    "samples(%d): %s\n",
    colnames(object)
  )
  S4Vectors::coolcat(
    "additional sample information(%d): %s\n",
    extraColDataNames(object)
  )
}

#' Show Method for `AnyHermesData` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A show method that displays high-level information of [`AnyHermesData`] objects.
#'
#' @rdname show
#' @aliases show
#'
#' @param object (`AnyHermesData`)\cr input.
#' @return None (invisible `NULL`), only used for the side effect of printing to
#'   the console.
#'
#' @note The same method is used for both [`HermesData`] and [`RangedHermesData`]
#'   objects. We need to define this separately to have this method used instead of
#'   the one inherited from [`SummarizedExperiment::SummarizedExperiment`].
#'
#' @export
#'
#' @examples
#' object <- hermes_data
#' object
setMethod(
  f = "show",
  signature = "HermesData",
  definition = .show.AnyHermesData
)

#' @rdname show
setMethod(
  f = "show",
  signature = "RangedHermesData",
  definition = .show.AnyHermesData
)

# correlate ----

#' Generic Function for Correlation Calculations
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' New generic function to calculate correlations for one or two objects.
#'
#' @param object input of which the class will be used to decide the method.
#' @param ... additional arguments.
#'
#' @return Corresponding object that contains the correlation results.
#'
#' @seealso [pca_cor_samplevar] and [calc_cor] which are the methods included for this generic function.
#' @export
#' @examples
#' sample_cors <- correlate(hermes_data)
#' autoplot(sample_cors)
#'
#' pca_sample_var_cors <- correlate(calc_pca(hermes_data), hermes_data)
#' autoplot(pca_sample_var_cors)
setGeneric("correlate", function(object, ...) standardGeneric("correlate"))

# autoplot ----

setGeneric("autoplot")

# lapply ----

#' `lapply` method for `MultiAssayExperiment`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Apply a function on all experiments in an MAE.
#'
#' @rdname lapply
#' @aliases lapply
#'
#' @param X (`MultiAssayExperiment`)\cr input.
#' @param FUN (`function`) to be applied to each experiment in `X`.
#' @param safe (`flag`)\cr whether this method should skip experiments
#'   where the function fails.
#' @param ... additional arguments passed to `FUN`.
#'
#' @return `MultiAssayExperiment` object with specified function applied.
#'
#' @importMethodsFrom BiocGenerics lapply
#' @export
#'
#' @examples
#' object <- multi_assay_experiment
#' result <- lapply(object, normalize, safe = TRUE)
#' # Similarly, all experiments in an MAE can be converted to HermesData class:
#' result <- lapply(object, HermesData, safe = TRUE)
setMethod(
  f = "lapply",
  signature = "MultiAssayExperiment",
  definition = function(X, FUN, safe = TRUE, ...) {
    assert_function(FUN)
    assert_flag(safe)
    FUN2 <- if (safe) {
      purrr::possibly(FUN, otherwise = NULL)
    } else {
      FUN
    }
    experiments(X) <- S4Vectors::endoapply(experiments(X), FUN2, ...)
    exp_lengths <- lengths(experiments(X))
    if (any(exp_lengths == 0)) {
      null_experiments <- experiments(X)[exp_lengths == 0]
      warning(
        "Specified function failed on ", toString(names(null_experiments))
      )
    }
    experiments(X) <- experiments(X)[exp_lengths > 0]
    X
  }
)

# isEmpty ----

#' Checking for Empty `SummarizedExperiment`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This method checks whether a [`SummarizedExperiment::SummarizedExperiment`] object is empty.
#'
#' @rdname isEmpty
#' @aliases isEmpty
#'
#' @param x (`SummarizedExperiment`)\cr object to check.
#'
#' @return Flag whether the `object` is empty.
#'
#' @importFrom S4Vectors isEmpty
#' @export
#'
#' @examples
#' isEmpty(summarized_experiment)
#' isEmpty(summarized_experiment[NULL, ])
#' isEmpty(hermes_data)
setMethod(
  f = "isEmpty",
  signature = "SummarizedExperiment",
  definition = function(x) {
    dims <- dim(x)
    isTRUE(any(dims == 0))
  }
)
