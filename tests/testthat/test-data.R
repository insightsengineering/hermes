test_that("expression_set can be used", {
  dat <- expect_silent(expression_set)
  expect_is(dat, "ExpressionSet")
})

test_that("summarized_experiment can be used", {
  dat <- expect_silent(summarized_experiment)
  expect_is(dat, "SummarizedExperiment")
})
