
library(rnaseqTools)
library(assertthat)
library(testthat)

# Tests

test_that("on_failure(is) error message is correct", {
  a <- 5
    expect_error(assert_that(is(a, "character"))
  , "a is not of class character")
  
})




