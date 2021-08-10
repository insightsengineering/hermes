# calc_cor ----

test_that("calc_cor function works as expected with default settings", {
  object <- HermesData(summarized_experiment)
  result <- expect_silent(correlate(object))
  expect_s4_class(result, "HermesDataCor")
  expect_named(result@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))
})

test_that("calc_cor function works as expected with custom settings", {
  object <- HermesData(summarized_experiment)
  result1 <- expect_silent(correlate(object, method = "spearman"))
  expect_s4_class(result1, "HermesDataCor")
  expect_named(result1@flag_data, c("TechnicalFailureFlag", "LowDepthFlag"))

  result2 <- correlate(object)
  expect_false(identical(result1, result2))
})

test_that("calc_cor fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  se <- get_se()
  expect_error(correlate(se))
  expect_error(correlate(object, method = "foo"))
  expect_error(correlate(object, assay_name = 1))
})

# autoplot-HermesDataCor ----

test_that("autoplot method does not give warnings on HermesDataCor objects", {
  hd <- HermesData(summarized_experiment)
  object <- correlate(hd)
  expect_s4_class(object, "HermesDataCor")
  result <- expect_silent(autoplot(object))
  expect_s4_class(result, "Heatmap")
})
