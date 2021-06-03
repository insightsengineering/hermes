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
  expect_true(is_hermes_data(HermesData(summarized_experiment)))
})

test_that("assertion with is_hermes_data gives readable error message", {
  a <- 5
  expect_error(
    assert_that(is_hermes_data(a)), 
    "a is not a HermesData or RangedHermesData object"
  )
})
