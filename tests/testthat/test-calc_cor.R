# calc_cor ----

test_that("calc_cor function works as expected with default settings", {
  object <- HermesData(summarized_experiment)
  result <- expect_silent(calc_cor(object))
  expect_s4_class(result, "HermesDataCor")
  expect_named(result@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
})

test_that("calc_cor function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  result1 <- expect_silent(calc_cor(object, method = "spearman"))
  expect_s4_class(result1, "HermesDataCor")
  expect_named(result1@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
  
  result2 <- calc_cor(object)
  expect_false(identical(result1, result2))
})

test_that("calc_cor fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  se <- get_se()
  expect_error(calc_cor(se))
  expect_error(calc_cor(object, method = "foo"))
  expect_error(calc_cor(object, assay_name = 1))
})
