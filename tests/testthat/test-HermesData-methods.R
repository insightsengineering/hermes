# rbind ----

test_that("rbind function works as expected for HermesData objects", {
  object <- get_se()
  h1 <- .HermesData(object[1])
  h2 <- .HermesData(object[2])
  h3 <- .HermesData(object)
  result <- expect_silent(rbind(h1, h2))
  expect_is(result, "HermesData")
  expect_equal(dim(result), dim(h3))
  expect_equal(rowData(result), rowData(h3))
  expect_equal(colData(result), colData(h3))
})

test_that("rbind function works as expected when binding SummarizedExperiment with HermesData", {
  object <- get_se()
  h1 <- .HermesData(object)
  result1 <- expect_silent(rbind(object, h1))
  expect_is(result1, "SummarizedExperiment")
  result2 <- expect_silent(rbind(h1, object))
  expect_is(result2, "SummarizedExperiment")
})

# cbind ----

test_that("cbind function works as expected for HermesData objects", {
  object <- get_se()
  h1 <- .HermesData(object[, 1])
  h2 <- .HermesData(object[, 2])
  h3 <- .HermesData(object)
  result <- expect_silent(cbind(h1, h2))
  expect_is(result, "HermesData")
  expect_equal(dim(result), dim(h3))
  expect_equal(rowData(result), rowData(h3))
  expect_equal(colData(result), colData(h3))
})

test_that("cbind function works as expected when binding SummarizedExperiment with HermesData", {
  object <- get_se()
  h1 <- .HermesData(object)
  result1 <- expect_silent(cbind(object, h1))
  expect_is(result1, "SummarizedExperiment")
  result2 <- expect_silent(cbind(h1, object))
  expect_is(result2, "SummarizedExperiment")
})

# metadata ----

test_that("metadata accessor works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  result <- expect_silent(metadata(h1))
  expected <- list(
    filename = "bla.txt",
    hash = "9352983502"
  )
  expect_identical(result, expected)
})

test_that("metadata setter works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  value <- list(a = "foo")
  expect_silent(metadata(h1) <- value)
  expect_identical(metadata(h1), value)
})

# counts ----

test_that("counts accessor works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  result <- expect_silent(counts(h1))
  expect_is(result, "matrix")
  expect_identical(dim(result), dim(h1))
})

test_that("counts setter works as expected", {
  object <- get_se()
  h1 <- .HermesData(object)
  value <- matrix(0L, nrow = nrow(h1), ncol = ncol(h1))
  expect_silent(counts(h1) <- value)
  expect_equivalent(counts(h1), value)
})

# subset ----

test_that("subset function works as expected for HermesData objects", {
  h <- .HermesData(get_se())
  result <- expect_silent(subset(
    h,
    subset = LowExpressionFlag,
    select = !TechnicalFailureFlag
  ))
  expect_is(result, "HermesData")
  expect_identical(nrow(result), sum(rowData(h)$LowExpressionFlag))
  expect_identical(ncol(result), sum(!h$TechnicalFailureFlag))
  expect_true(all(rowData(result)$LowExpressionFlag))
  expect_true(all(!result$TechnicalFailureFlag))
})


# filter ----

test_that("filter works as expected for HermesData", {
  object <- get_se()
  h1 <- HermesData(object)
  result <- expect_silent(filter(h1))
  expect_is(result, "HermesData")
  # Only one gene, but no samples fulfill filter criteria:
  expect_identical(dim(result), c(1L, 0L))
})

test_that("filter works as expected for RangedHermesData", {
  object <- get_rse()
  h1 <- HermesData(object)
  result <- expect_silent(filter(h1))
  expect_is(result, "RangedHermesData")
  # Only one gene, but no samples fulfill filter criteria:
  expect_identical(dim(result), c(1L, 0L))
})

test_that("filter shows readable error message when there are NA in flag variables", {
  object <- get_se()
  object$LowDepthFlag[1] <- NA
  h1 <- HermesData(object)
  expect_error(
    filter(h1),
    "still NA in quality flags, please first run add_quality_flags() to fill them",
    fixed = TRUE
  )
})

# summary ----

test_that("summary works as expected for HermesData", {
  object <- HermesData(summarized_experiment)
  cd <- as.data.frame(colData(object))
  rd <- as.data.frame(rowData(object))
  result <- expect_silent(summary(object))
  expect_is(result, "HermesDataSummary")
  expect_identical(result@class_name, class(object)[[1]])
  expect_identical(result@assay_names, assayNames(object))
  expect_identical(result@n_genes, nrow(object))
  expect_identical(result@n_samples, ncol(object))
  expect_identical(length(result@genes_fail), sum(rd$LowExpressionFlag))
  expect_identical(length(result@samples_fail), sum(cd$TechnicalFailureFlag, cd$LowDepthFlag))
})

test_that("summary works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  cd <- as.data.frame(colData(object))
  rd <- as.data.frame(rowData(object))
  result <- expect_silent(summary(object))
  expect_is(result, "HermesDataSummary")
  expect_identical(result@class_name, class(object)[[1]])
  expect_identical(result@assay_names, assayNames(object))
  expect_identical(result@n_genes, nrow(object))
  expect_identical(result@n_samples, ncol(object))
  expect_identical(length(result@genes_fail), sum(rd$LowExpressionFlag))
  expect_identical(length(result@samples_fail), sum(cd$TechnicalFailureFlag, cd$LowDepthFlag))
})

# show-summary ----

test_that("show for summary works as expected for HermesData with quality flags", {
  object <- summary(HermesData(summarized_experiment))
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
  rowData(se)$LowExpressionFlag <- NA # nolint
  colData(se)$LowDepthFlag <- NA # nolint
  colData(se)$TechnicalFailureFlag <- NA # nolint
  object <- summary(HermesData(se))
  result <- capture_output(show(object))
  expect_match(
    result,
    "- QC flags still need to be added"
  )
})

# show ----

test_that("show works as expected for HermesData", {
  object <- HermesData(summarized_experiment)
  result <- capture_output(show(object))
  expect_match(result, "class: HermesData", fixed = TRUE)
  expect_match(result, "assays(1): counts", fixed = TRUE)
  expect_match(result, "genes(5085):", fixed = TRUE)
  expect_match(result, "samples(20):", fixed = TRUE)
  expect_match(result, "additional gene information(2):", fixed = TRUE)
  expect_match(result, "additional sample information(84):", fixed = TRUE)
})

test_that("show works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  result <- capture_output(show(object))
  expect_match(result, "class: RangedHermesData", fixed = TRUE)
  expect_match(result, "genes(2):", fixed = TRUE)
  expect_match(result, "additional gene information(1):", fixed = TRUE)
  expect_match(result, "additional sample information(0):", fixed = TRUE)
})
