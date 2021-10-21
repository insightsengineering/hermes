# calc_cor ----

test_that("calc_cor function works as expected with default settings", {
  object <- hermes_data
  result <- expect_silent(correlate(object))
  expect_s4_class(result, "HermesDataCor")
  expect_named(result@flag_data, c("tech_failure_flag", "low_depth_flag"))
})

test_that("calc_cor function works as expected with custom settings", {
  object <- hermes_data
  result1 <- expect_silent(correlate(object, method = "spearman"))
  expect_s4_class(result1, "HermesDataCor")
  expect_named(result1@flag_data, c("tech_failure_flag", "low_depth_flag"))

  result2 <- correlate(object)
  expect_false(identical(result1, result2))
})

test_that("calc_cor fails as expected with invalid settings", {
  object <- hermes_data
  se <- get_se()
  expect_error(correlate(se))
  expect_error(correlate(object, method = "foo"))
  expect_error(correlate(object, assay_name = 1))
})

# autoplot-HermesDataCor ----

test_that("autoplot method does not give warnings on HermesDataCor objects", {
  hd <- hermes_data
  object <- correlate(hd)
  expect_s4_class(object, "HermesDataCor")
  result <- expect_silent(autoplot(object))
  expect_s4_class(result, "Heatmap")
})
