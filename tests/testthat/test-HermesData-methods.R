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
  h1 <- .HermesData(object[,1])
  h2 <- .HermesData(object[,2])
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
