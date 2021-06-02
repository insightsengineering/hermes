# control_normalize ----

test_that("control_normalize function works as expected", {
  control1 <- control_normalize()
  control2 <- control_normalize(log = T, lib_size = 60000000L, prior_count = 3)
   
  expect_is(control1, "list")
  expect_is(control2, "list")
  expect_error(control_normalize(log = "a"))
  expect_error(control_normalize(lib_size = 1))
  expect_error(control_normalize(prior_count = -1))
})
