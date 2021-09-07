# df_char_to_factor ----

test_that("df_char_to_factor works as expected with default settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA)
  )
  result <- df_char_to_factor(dat)
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

test_that("h_parens works as expected", {
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

# colPinComp1 ----

test_that("colPinComp1 function works as expected for HermesData with default counts assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  pca <- expect_silent(colPrinComp1(result))
  expect_vector(pca)
  expect_is(pca, "numeric")
  expect_equal(length(pca), ncol(object))
  expect_lt(mean(pca), 1e-10)
})

test_that("colPinComp1 function returns an error when the assay name does not exist", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  expect_error(colPrinComp1(result,"a_fake_column"))
})

# colMeanZScores ----

test_that("colMeanZScores function works as expected for HermesData with default counts assay", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  score <- expect_silent(colMeanZscores(result))
  expect_vector(score)
  expect_is(score, "numeric")
  expect_equal(length(score), ncol(object))
  expect_lt(mean(score), 1e-10)
})

test_that("colMeanZScores function returns an error when the assay name does not exist", {
  object <- expect_silent(HermesData(summarized_experiment))
  result <- expect_silent(normalize(object))
  expect_error(colMeanZscores(result,"a_fake_column"))
})



