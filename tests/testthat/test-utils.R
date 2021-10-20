# df_char_to_factor ----

test_that("df_char_to_factor works as expected with default settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA)
  )
  result <- expect_warning(df_char_to_factor(dat), "deprecated")
  expected <- S4Vectors::DataFrame(
    a = factor(c("<Missing>", "B"), levels = c("B", "<Missing>")),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = factor(c("U", "<Missing>"), levels = c("U", "<Missing>"))
  )
  expect_identical(result, expected)
})

test_that("df_char_to_factor works as expected with custom settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA)
  )
  result <- df_char_to_factor(dat, omit_columns = c("a", "b"), na_level = "Misses")
  expected <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = factor(c("U", "Misses"), levels = c("U", "Misses"))
  )
  expect_identical(result, expected)
})

# df_cols_to_factor ----

test_that("df_cols_to_factor works as expected with default settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA),
    chromosome = c("a", "b"),
    low_depth_flag = c(TRUE, FALSE)
  )
  result <- df_cols_to_factor(dat)
  expected <- S4Vectors::DataFrame(
    a = factor(c("<Missing>", "B"), levels = c("B", "<Missing>")),
    b = array(1:4, c(2, 1, 2)),
    c = factor(c(TRUE, FALSE)),
    d = factor(c("X", NA)),
    e = factor(c("U", "<Missing>"), levels = c("U", "<Missing>")),
    chromosome = c("a", "b"),
    low_depth_flag = c(TRUE, FALSE)
  )
  expect_identical(result, expected)
})

test_that("df_cols_to_factor works as expected with custom settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, NA),
    d = factor(c("X", NA)),
    e = c("U", NA),
    chromosome = c("a", "b"),
    low_depth_flag = c(TRUE, FALSE)
  )
  result <- df_cols_to_factor(dat, omit_columns = c("a", "b"), na_level = "Misses")
  expected <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = factor(c(TRUE, "Misses"), levels = c("TRUE", "Misses")),
    d = factor(c("X", NA)),
    e = factor(c("U", "Misses"), levels = c("U", "Misses")),
    chromosome = c("a", "b"),
    low_depth_flag = c(TRUE, FALSE)
  )
  expect_identical(result, expected)
})

# h_short_list ----

test_that("h_short_list works as expected with default arguments", {
  expect_identical(
    h_short_list(letters),
    "a, b, ..., z"
  )
  expect_identical(
    h_short_list(letters[1:3]),
    "a, b, c"
  )
  expect_identical(
    h_short_list(letters[1:2]),
    "a, b"
  )
  expect_identical(
    h_short_list(letters[1]),
    "a"
  )
})

test_that("h_short_list works as expected with custom arguments", {
  expect_identical(
    h_short_list(letters, sep = "; "),
    "a; b; ...; z"
  )
  expect_identical(
    h_short_list(letters[1:4], thresh = 4L),
    "a, b, c, d"
  )
  expect_identical(
    h_short_list(letters[1:5], thresh = 5L, sep = "-"),
    "a-b-c-d-e"
  )
  expect_error(h_short_list(letters[1:3], thresh = 2L))
  expect_error(h_short_list(letters[1:3], sep = ""))
})

# h_parens ----

test_that("h_parens works as expected for strings", {
  expect_identical(
    h_parens("bla"),
    "(bla)"
  )
  expect_identical(
    h_parens(""),
    ""
  )
  expect_error(h_parens(123))
  expect_error(h_parens(NULL))
})

test_that("h_parens works as expected for character vectors", {
  expect_identical(
    h_parens(c("bla", "bli")),
    c("(bla)", "(bli)")
  )
  expect_identical(
    h_parens(c("", "bla")),
    c("", "(bla)")
  )
  expect_error(h_parens(c(NA, "")))
})

# colPrinComp1 ----

test_that("colPrinComp1 function works as expected for standard matrix input", {
  set.seed(123)
  cnts <- matrix(data = rpois(30, 5), nrow = 3, ncol = 10)
  cnts[2L, ] <- 5  # Constant gene.
  result <- expect_silent(colPrinComp1(cnts))
  expect_numeric(result)
  expect_length(result, 10L)
  expect_lt(mean(result), 1e-10)
})

test_that("colPrinComp1 function returns an error when data are not numeric", {
  cnts <- expect_silent(matrix(as.character(1:20), 4, 5))
  expect_error(colPrinComp1(cnts))
})

# colMeanZScores ----

test_that("colMeanZScores function works as expected for standard matrix input", {
  set.seed(123)
  cnts <- matrix(data = rpois(30, 5), nrow = 3, ncol = 10)
  cnts[2L, ] <- 5  # Constant gene.
  result <- expect_silent(colMeanZscores(cnts))
  expect_numeric(result)
  expect_length(result, 10L)
  expect_lt(mean(result), 1e-10)
})

test_that("colMeanZScores function returns an error when data are not numeric", {
  result <- expect_silent(matrix(as.character(1:20), 4, 5))
  expect_error(colMeanZScores(result))
})

# wrap_in_mae ----

test_that("wrap_in_mae function works as expected for standard SummarizedExperiment object", {
  object <- summarized_experiment
  result <- expect_silent(wrap_in_mae(object))
  expect_is(result, "MultiAssayExperiment")
  expect_identical(result[[1]], object)
})

test_that("wrap_in_mae function uses the correct default name for the experiment", {
  result <- expect_silent(wrap_in_mae(summarized_experiment))
  expect_named(result, "summarized_experiment")
})

test_that("wrap_in_mae function uses the custom name for the experiment", {
  result <- expect_silent(wrap_in_mae(
    summarized_experiment,
    name = "bla"
  ))
  expect_named(result, "bla")
})

# h_all_duplicated ----

test_that("h_all_duplicated works as expected for numerics", {
  result <- expect_silent(h_all_duplicated(c(1, 1, 2, 3, 3, 4)))
  expected <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_identical(result, expected)
})

test_that("h_all_duplicated works as expected for character", {
  result <- expect_silent(h_all_duplicated(c("a", "", "", "a", "c")))
  expected <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
  expect_identical(result, expected)
})

# cut_quantile ----

test_that("cut_quantile works as expected with default settings", {
  x <- c(4, 6, -10, 2.2, 0)
  result <- cut_quantile(x)
  expect_identical(as.character(result), c("(67%,100%]", "(67%,100%]", "[0%,33%]", "(33%,67%]", "[0%,33%]"))
  expect_equal(length(levels(result)), 3)
  expect_identical(length(x), length(result))
  expect_true(is.factor(result))
})

test_that("cut_quantile works as expected with custom settings", {
  x <- c(4, 6, -10, 2.2, 0)
  result <- cut_quantile(x, c(0.33333333, 0.6666666, 0.8), digits = 3)
  expect_identical(as.character(result), c("(66.7%,80%]", "(80%,100%]", "[0%,33.3%]", "(33.3%,66.7%]", "[0%,33.3%]"))
  expect_equal(length(levels(result)), 4)
  expect_identical(length(x), length(result))
  expect_true(is.factor(result))
})

test_that("cut_quantile preserves NAs in result", {
  x <- c(4, 6, -10, 2.2, 0)
  x[c(1, 4)] <- NA
  result <- cut_quantile(x)
  expect_identical(is.na(x), is.na(result))
  result2 <- cut_quantile(x, percentiles = c(0.2, 0.7, 0.8))
  expect_identical(is.na(x), is.na(result2))
})

test_that("cut_quantile works with NULL percentile", {
  x <- c(4, 6, -10, 2.2, 0)
  result <- cut_quantile(x, c())
  expect_identical(levels(result), "[0%,100%]")
})

test_that("cut_quantile produces an error if quantile boundaries are not unique", {
  x <- rep(1, 10)
  expect_error(cut_quantile(x))
})
