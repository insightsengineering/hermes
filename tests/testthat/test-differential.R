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

# h_diff_expr_deseq2 ----

test_that("h_diff_expr_deseq2 works as expected", {
  object <- HermesData(summarized_experiment)
  design <- model.matrix(~ SEX, colData(object))
  result <- h_diff_expr_deseq2(object, design)
  expect_is(result, "data.frame")
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$adj_p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("h_diff_expr_deseq2 fails if design matrix is not correct", {
  object <- HermesData(summarized_experiment)
  design_too_wide <- model.matrix(~ SEX + COUNTRY, colData(object))
  expect_error(h_diff_expr_voom(object, design_too_wide))
  design_diff_obs <- model.matrix(~ SEX, colData(object)[1:10, ])
  expect_error(h_diff_expr_deseq2(object, design_diff_obs))
})

# diff_expression ----
test_that("diff_expression works as expected", {
  object <- HermesData(summarized_experiment)
  colData(object)$SEX <- factor(colData(object)$SEX)
  result1 <- expect_silent(diff_expression(object, "SEX", "deseq2"))
  result2 <- expect_silent(diff_expression(object, "SEX", "limma_voom"))
  expect_is(result1, "HermesDataDiffExpr")
  expect_is(result2, "HermesDataDiffExpr")
  expect_named(result1, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_named(result2, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result1$adj_p_val))
  expect_true(S4Vectors::isSorted(result2$adj_p_val))
  expect_setequal(rownames(object), rownames(result1))
  expect_setequal(rownames(object), rownames(result2))
})

test_that("diff_expression fails as expected with inappropriate inputs", {
  object <- HermesData(summarized_experiment)
  gse <- get_se()
  expect_error(diff_expression(object, "COUNRTY", "deseq2"))
  expect_error(diff_expression(object, "COUNRTY", "anbc"))
  expect_error(diff_expression(gse, "SEX", "deseq2"))
})

