# is_class ----

test_that("is_class works correctly", {
  expect_false(is_class("bla", "numeric"))
  expect_true(is_class(5, "numeric"))
})

test_that("assertion with is_class gives readable error message", {
  a <- 5
  expect_error(assert_that(is_class(a, "character")))
})