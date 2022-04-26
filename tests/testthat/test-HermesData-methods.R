# rbind ----

test_that("rbind function works as expected for HermesData objects", {
  object <- get_se()
  h1 <- HermesData(object[1])
  h2 <- HermesData(object[2])
  h3 <- HermesData(object)
  result <- expect_silent(rbind(h1, h2))
  expect_is(result, "HermesData")
  expect_equal(dim(result), dim(h3))
  expect_equal(rowData(result), rowData(h3))
  expect_equal(colData(result), colData(h3))
})

test_that("rbind function works as expected when binding SummarizedExperiment with HermesData", {
  object <- get_se()
  se <- object[1]
  h1 <- HermesData(object[2])
  result1 <- expect_silent(rbind(se, h1))
  expect_is(result1, "SummarizedExperiment")
  result2 <- expect_silent(rbind(h1, se))
  expect_is(result2, "SummarizedExperiment")
})

test_that("rbind function fails as expected when rbind results in duplicated rownames", {
  object <- hermes_data
  expect_error(rbind(object, object))
})

# cbind ----

test_that("cbind function works as expected for HermesData objects", {
  object <- get_se()
  h1 <- HermesData(object[, 1])
  h2 <- HermesData(object[, 2])
  h3 <- HermesData(object)
  result <- expect_silent(cbind(h1, h2))
  expect_is(result, "HermesData")
  expect_equal(dim(result), dim(h3))
  expect_equal(rowData(result), rowData(h3))
  expect_equal(colData(result), colData(h3))
})

test_that("cbind function works as expected when binding SummarizedExperiment with HermesData", {
  object <- get_se()
  se <- object[, 1]
  h1 <- HermesData(object[, 2])
  result1 <- expect_silent(cbind(se, h1))
  expect_is(result1, "SummarizedExperiment")
  result2 <- expect_silent(cbind(h1, se))
  expect_is(result2, "SummarizedExperiment")
})

test_that("rbind function fails as expected when rbind results in duplicated colnames", {
  object <- hermes_data
  expect_error(cbind(object, object))
})

# metadata ----

test_that("metadata accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(metadata(h1))
  expected <- list(
    filename = "bla.txt",
    hash = "9352983502"
  )
  expect_identical(result, expected)
})

test_that("metadata setter works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  value <- list(a = "foo")
  expect_silent(metadata(h1) <- value)
  expect_identical(metadata(h1), value)
})

# annotation ----

test_that("annotation accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(annotation(h1))
  expect_s4_class(result, "DataFrame")
  expect_named(result, .row_data_annotation_cols)
  expect_identical(rownames(result), rownames(h1))
})

test_that("annotation setter works as expected when the columns are set equal", {
  object <- get_se()
  h1 <- HermesData(object)
  value <- S4Vectors::DataFrame(
    symbol = c(1, 1),
    desc = c(1, 1),
    size = c(11, 1),
    chromosome = c(1, 1),
    row.names = c("GeneID:a", "GeneID:b")
  )
  expect_silent(annotation(h1) <- value)
  # Internally we expect a reordering of the columns to take place.
  expect_false(identical(names(annotation(h1)), names(value)))
  expect_setequal(names(annotation(h1)), names(value))
})

test_that("annotation setter works also when there are more columns provided in the value", {
  object <- get_se()
  h1 <- HermesData(object)
  value <- S4Vectors::DataFrame(
    symbol = c(1, 1),
    desc = c(1, 1),
    size = c(11, 1),
    chromosome = c(1, 1),
    protein_information = c("A", "C"),
    important_others = c("D", "E"),
    row.names = c("GeneID:a", "GeneID:b")
  )
  expect_silent(annotation(h1) <- value)
  # Internally we expect a reordering of the columns to take place.
  expect_false(identical(names(annotation(h1)), names(value)[1:4]))
  expect_setequal(names(annotation(h1)), names(value)[1:4])
  expect_subset(names(value), names(rowData(h1)))
})

test_that("annotation setter gives a warning, saves gene IDs in attribute if gene info is missing", {
  object <- get_se()
  h1 <- HermesData(object)
  # Value where information for one gene is completely missing, only partially missing for the other.
  value <- S4Vectors::DataFrame(
    symbol = c(NA, 1),
    desc = c(NA, NA),
    size = c(NA, 1),
    chromosome = c(NA, 1),
    row.names = c("GeneID:a", "GeneID:b")
  )
  expect_warning(
    annotation(h1) <- value,
    "completely missing for 1 genes"
  )
  expect_identical(attr(h1, "annotation.missing.genes"), "GeneID:a")
})

# counts ----

test_that("counts accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(counts(h1))
  expect_is(result, "matrix")
  expect_identical(dim(result), dim(h1))
})

test_that("counts setter works as expected with `withDimnames = FALSE`", {
  object <- get_se()
  h1 <- HermesData(object)
  value <- matrix(0L, nrow = nrow(h1), ncol = ncol(h1))
  expect_silent(counts(h1, withDimnames = FALSE) <- value)
  expect_equivalent(counts(h1), value)
})

# prefix ----

test_that("prefix accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(prefix(h1))
  expect_identical(result, "GeneID")
})

# genes ----

test_that("genes accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(genes(h1))
  expect_identical(result, c("GeneID:a", "GeneID:b"))
})

# samples ----

test_that("samples accessor works as expected", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(samples(h1))
  expect_identical(result, c("X", "Y"))
})

# subset ----

test_that("subset function works as expected for HermesData objects", {
  h <- HermesData(get_se())
  result <- expect_silent(subset(
    h,
    subset = low_expression_flag,
    select = !tech_failure_flag
  ))
  expect_is(result, "HermesData")
  expect_identical(nrow(result), sum(rowData(h)$low_expression_flag))
  expect_identical(ncol(result), sum(!h$tech_failure_flag))
  expect_true(all(rowData(result)$low_expression_flag))
  expect_true(all(!result$tech_failure_flag))
})

# h_has_req_annotations ----

test_that("h_has_req_annotations works as expected", {
  object <- get_se()

  h1 <- HermesData(object)
  result1 <- h_has_req_annotations(h1, .row_data_annotation_cols)
  expected1 <- c("GeneID:a" = TRUE, "GeneID:b" = TRUE)
  expect_identical(result1, expected1)

  h2 <- h1
  rowData(h2)$size[1] <- NA # nolint
  result2 <- h_has_req_annotations(h2, .row_data_annotation_cols)
  expected2 <- c("GeneID:a" = FALSE, "GeneID:b" = TRUE)
  expect_identical(result2, expected2)
})

# filter ----

test_that("filter works as expected with default settings for HermesData", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(filter(h1))
  expect_is(result, "HermesData")
  # Only one gene and one sample fulfill filter criteria:
  expect_identical(dim(result), c(1L, 1L))
})

test_that("filter works as expected with default settings for RangedHermesData", {
  object <- get_rse()
  h1 <- HermesData(object)
  result <- expect_silent(filter(h1))
  expect_is(result, "RangedHermesData")
  # Only one gene and one sample fulfill filter criteria:
  expect_identical(dim(result), c(1L, 1L))
})

test_that("filter works as expected on one dimension for HermesData", {
  object <- hermes_data
  result1 <- expect_silent(filter(object, what = "genes"))
  result2 <- expect_silent(filter(object, what = "samples"))
  expect_is(result1, "HermesData")
  expect_is(result2, "HermesData")
  expect_identical(ncol(result1), ncol(object))
  expect_identical(nrow(result2), nrow(object))
})

test_that("filter works as expected with default settings for RangedHermesData", {
  object <- get_rse()
  h1 <- HermesData(object)
  result <- expect_silent(filter(h1))
  expect_is(result, "RangedHermesData")
  # Only one gene and one sample fulfill filter criteria:
  expect_identical(dim(result), c(1L, 1L))
})

test_that("filter shows readable error message when there are NA in flag variables", {
  object <- get_se()
  object$low_depth_flag[1] <- NA
  h1 <- HermesData(object)
  expect_error(
    filter(h1),
    "still NA in quality flags, please first run add_quality_flags() to fill them",
    fixed = TRUE
  )
})

test_that("filter gives a warning if all samples are filtered out", {
  object <- get_se()
  object$low_depth_flag <- TRUE # nolint
  h <- HermesData(object)
  expect_warning(
    filter(h),
    "filtering out all samples"
  )
})

test_that("filter gives a warning if all genes are filtered out", {
  object <- get_se()
  rowData(object)$low_expression_flag <- TRUE # nolint
  h <- HermesData(object)
  expect_warning(
    filter(h),
    "filtering out all genes"
  )
})

test_that("filter by default correctly filters out genes which don't have required `size`", {
  object <- get_se()
  rowData(object)$low_expression_flag <- FALSE # nolint
  rowData(object)$size[1] <- NA # nolint
  h <- HermesData(object)
  result <- filter(h)
  expect_identical(genes(result), "GeneID:b")
  expect_true(noNA(annotation(result)$size))
})

# h_map_pos ----

test_that("h_map_pos works as expected", {
  result <- expect_silent(h_map_pos(c("a", "b"), c(d = "b")))
  expected <- 2L
  expect_identical(result, expected)
})

test_that("h_map_pos fails as expected when old names are not found", {
  expect_error(
    h_map_pos(c("a", "b"), c(d = "b", e = "z")),
    "Must be a subset of {'a','b'}, but has additional elements {'z'}",
    fixed = TRUE
  )
})

# rename ----

test_that("rename works with rowData columns", {
  x <- summarized_experiment
  pos <- match("HGNC", names(rowData(x)))
  assert_count(pos)
  result <- rename(x, row_data = c(symbol = "HGNC"))
  expect_true(identical(names(rowData(result))[pos], "symbol"))
})

test_that("rename works with colData columns", {
  x <- summarized_experiment
  pos <- match("LowDepthFlag", names(colData(x)))
  assert_count(pos)
  result <- rename(x, col_data = c(low_depth_flag = "LowDepthFlag"))
  expect_true(identical(names(colData(result))[pos], "low_depth_flag"))
})

test_that("rename works with assay names", {
  x <- summarized_experiment
  assert_names(assayNames(x), identical.to = "counts")
  result <- rename(x, assays = c(count = "counts"))
  expect_names(assayNames(result), identical.to = "count")
})

# summary ----

test_that("summary works as expected for HermesData", {
  object <- hermes_data
  cd <- as.data.frame(colData(object))
  rd <- as.data.frame(rowData(object))
  result <- expect_silent(summary(object))
  expect_s4_class(result, "HermesDataSummary")
  expect_identical(result@class_name, "HermesData")
  expect_identical(result@assay_names, assayNames(object))
  expect_identical(result@n_genes, nrow(object))
  expect_identical(result@n_samples, ncol(object))
  expect_identical(length(result@genes_fail), sum(rd$low_expression_flag))
  expect_identical(length(result@samples_fail), sum(cd$tech_failure_flag, cd$low_depth_flag))
})

test_that("summary works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  cd <- as.data.frame(colData(object))
  rd <- as.data.frame(rowData(object))
  result <- expect_silent(summary(object))
  expect_s4_class(result, "HermesDataSummary")
  expect_identical(result@class_name, "RangedHermesData")
  expect_identical(result@assay_names, assayNames(object))
  expect_identical(result@n_genes, nrow(object))
  expect_identical(result@n_samples, ncol(object))
  expect_identical(length(result@genes_fail), sum(rd$low_expression_flag))
  expect_identical(length(result@samples_fail), sum(cd$tech_failure_flag, cd$low_depth_flag))
})

# show-summary ----

test_that("show for summary works as expected for HermesData with quality flags", {
  object <- summary(hermes_data)
  result <- capture_output(show(object))
  expect_match(
    result,
    "^HermesData object with 20 samples of 5085 genes.\n- Library sizes"
  )
  expect_match(
    result,
    "Samples with too low depth or technical failures (2)",
    fixed = TRUE
  )
})

test_that("show for summary also works without quality flags", {
  se <- get_se()
  rowData(se)$low_expression_flag <- NA # nolint
  colData(se)$low_depth_flag <- NA # nolint
  colData(se)$tech_failure_flag <- NA # nolint
  object <- summary(HermesData(se))
  result <- capture_output(show(object))
  expect_match(
    result,
    "- QC flags still need to be added"
  )
})

# show ----

test_that("show works as expected for HermesData", {
  object <- hermes_data
  result <- capture_output(show(object))
  expect_match(result, "class: HermesData", fixed = TRUE)
  expect_match(result, "assays(1): counts", fixed = TRUE)
  expect_match(result, "genes(5085):", fixed = TRUE)
  expect_match(result, "samples(20):", fixed = TRUE)
  expect_match(result, "additional gene information(3):", fixed = TRUE)
  expect_match(result, "additional sample information(72):", fixed = TRUE)
})

test_that("show works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  result <- capture_output(show(object))
  expect_match(result, "class: RangedHermesData", fixed = TRUE)
  expect_match(result, "genes(2):", fixed = TRUE)
  expect_match(result, "additional gene information(1):", fixed = TRUE)
  expect_match(result, "additional sample information(0):", fixed = TRUE)
})

# lapply ----

test_that("lapply works as expected for an MAE", {
  mae <- multi_assay_experiment
  result <- expect_message(lapply(mae, normalize))
  expect_is(result, "MultiAssayExperiment")
  expect_is(result[[1]], "HermesData")
  expect_is(result[[2]], "HermesData")
  expect_is(result[[3]], "HermesData")
  expect_equal(dim(mae[[1]]), dim(result[[1]]))
  expect_equal(dim(mae[[2]]), dim(result[[2]]))
  expect_equal(dim(mae[[3]]), dim(result[[3]]))
})

test_that("lapply works as expected with safe = TRUE argument when converting experiments in an MAE to HermesData", {
  mae <- multi_assay_experiment
  mae[[1]] <- rename(mae[[1]], assay = c(count = "counts"))
  result <- expect_warning(lapply(mae, HermesData), "Specified function failed on hd1")
  expect_is(result, "MultiAssayExperiment")
  expect_is(result[[1]], "HermesData")
  expect_is(result[[2]], "HermesData")
  expect_equal(dim(mae[[2]]), dim(result[[1]]))
  expect_equal(dim(mae[[3]]), dim(result[[2]]))
})

test_that("lapply fails as expected with safe = FALSE arugment when converting experiments in an MAE to HermesData", {
  mae <- multi_assay_experiment
  mae[[1]] <- rename(mae[[1]], assay = c(count = "counts"))
  expect_error(lapply(mae, HermesData, safe = FALSE), "invalid class")
})

# isEmpty ----

test_that("isEmpty works as expected", {
  expect_false(isEmpty(hermes_data))
  expect_false(isEmpty(summarized_experiment))
  expect_true(isEmpty(hermes_data[NULL, ]))
  expect_true(isEmpty(summarized_experiment[, NULL]))
  expect_true(isEmpty(summarized_experiment[NULL, NULL]))
})
