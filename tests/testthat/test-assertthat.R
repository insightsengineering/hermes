# is_class ----

test_that("is_class works correctly", {
  expect_false(is_class("bla", "numeric"))
  expect_true(is_class(5, "numeric"))
})

test_that("assertion with is_class gives readable error message", {
  a <- 5
  expect_error(
    assert_that(is_class(a, "character")),
    "a is not of class character"
  )
})

# is_hermes_data ----

test_that("is_hermes_data works correctly", {
  expect_false(is_hermes_data("bla"))
  expect_true(is_hermes_data(hermes_data))
})

test_that("assertion with is_hermes_data gives readable error message", {
  a <- 5
  expect_error(
    assert_that(is_hermes_data(a)),
    "a is not a HermesData or RangedHermesData object"
  )
})

# is_counts_vector ----

test_that("is_counts_vector accepts positive integer vectors", {
  expect_true(is_counts_vector(c(1L, 2L)))
  expect_true(is_counts_vector(5L))
})

test_that("is_counts_vector rejects other inputs as expected", {
  expect_false(is_counts_vector(c(1, 2)))
  expect_false(is_counts_vector(c(NA_integer_, 1L)))
  expect_false(is_counts_vector(c(0L, 1L)))
  expect_false(is_counts_vector(integer()))
  expect_false(is_counts_vector(list(1L, 2L)))
})

test_that("is_counts_vector gives readable error message", {
  a <- 5
  expect_error(
    assert_that(is_counts_vector(a)),
    "a is not a vector of counts (positive integers)",
    fixed = TRUE
  )
})

# is_list_with ----

test_that("is_list_with accepts lists with the required elements", {
  x <- list(a = 3, b = 5, c = NULL)
  expect_true(is_list_with(x, c("a", "b", "c")))
  expect_true(is_list_with(x, c("a", "b")))
  expect_true(is_list_with(x, c("b", "a")))
  expect_true(is_list_with(x, "c"))
})

test_that("is_list_with rejects non-lists or lists that are not fully or uniquely named", {
  expect_false(is_list_with(c(a = 3), "a"))
  expect_false(is_list_with(list(a = 3, 5), "a"))
  expect_false(is_list_with(list(a = 3, a = 5), "a"))
})

test_that("is_list_with rejects lists that don't contain all required elements", {
  expect_false(is_list_with(list(a = 3), "b"))
  expect_false(is_list_with(list(a = 3), c("a", "b")))
})

test_that("is_list_with gives readable error messages", {
  a <- list(a = 3, b = 5, c = NULL)
  expect_error(
    assert_that(is_list_with(a, "e")),
    "a is not a fully and uniquely named list containing all elements e"
  )
  expect_error(
    assert_that(is_list_with(a, c("a", "e"))),
    "a is not a fully and uniquely named list containing all elements a, e"
  )
})
