test_that("loading hermes works", {
  test.nest::skip_if_too_deep(0)

  expect_silent(library(hermes))
})
