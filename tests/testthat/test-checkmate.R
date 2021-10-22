# check_proportion ----

test_that("check_proportion works as expected", {
  expect_identical(
    check_proportion(2),
    "Must be a 'proportion': number between 0 and 1"
  )
  expect_identical(
    check_proportion(NULL),
    "Must be a 'proportion': number between 0 and 1"
  )
  expect_identical(
    check_proportion(c(0.5, 0.9)),
    "Must be a 'proportion': number between 0 and 1"
  )
  expect_true(
    check_proportion(0.5)
  )
})
