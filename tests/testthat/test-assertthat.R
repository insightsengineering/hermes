# is_class ----

test_that("is_class works correctly", {
  expect_false(is_class("bla", "numeric"))
  expect_true(is_class(5, "numeric"))
})

test_that("assertion with is_class gives readable error message", {
  a <- 5
  expect_error(assert_that(is_class(a, "character")))
})

# is_counts_vector

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
