# h_diff_expr_voom ----

test_that("h_diff_expr_voom works as expected", {
  object <- HermesData(summarized_experiment)
  design <- model.matrix(~ SEX, colData(object))
  result <- h_diff_expr_voom(object, design)
  expect_is(result, "data.frame")
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("h_diff_expr_voom fails if design matrix is not correct", {
  object <- HermesData(summarized_experiment)
  design_too_wide <- model.matrix(~ SEX + COUNTRY, colData(object))
  expect_error(h_diff_expr_voom(object, design_too_wide))
  design_diff_obs <- model.matrix(~ SEX, colData(object)[1:10, ])
  expect_error(h_diff_expr_voom(object, design_diff_obs))
})
