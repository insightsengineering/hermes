library(rnaseqTools)
library(assertthat)
library(testthat)

# Tests

testthat("on_failure(is) error message is correct", {
  a <- 5
  expect_error(assert_that(is(a, "character"))
  , "a is not of class character")
  
})




a <- 5
assert_that(is(a, "character"))

b <- 'b'
assert_that(is(b, "numeric"))

c <- 5554
assert_that(is(b, "gSummarizedExperiment"))

e <- 10
assert_that(is(e, "numeric"))


