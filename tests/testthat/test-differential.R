# h_diff_expr_voom ----

test_that("h_diff_expr_voom works as expected", {
  object <- hermes_data
  design <- model.matrix(~SEX, colData(object))
  result <- h_diff_expr_voom(object, design)
  expect_data_frame(result)
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("h_diff_expr_voom can pass arguments to limma::eBayes", {
  object <- hermes_data
  design <- model.matrix(~SEX, colData(object))
  result <- expect_silent(h_diff_expr_voom(
    object,
    design = design,
    # Use all limma::eBayes arguments below.
    robust = TRUE,
    trend = TRUE,
    proportion = 0.02,
    winsor.tail.p = c(0.02, 0.2),
    stdev.coef.lim = c(0.09, 5)
  ))
  expect_data_frame(result)
  result_orig <- h_diff_expr_voom(object, design)
  expect_false(identical(rownames(result), rownames(result_orig)))
})

test_that("h_diff_expr_voom fails if design matrix is not correct", {
  object <- hermes_data
  design_too_wide <- model.matrix(~ SEX + COUNTRY, colData(object))
  expect_error(h_diff_expr_voom(object, design_too_wide))
  design_diff_obs <- model.matrix(~SEX, colData(object)[1:10, ])
  expect_error(h_diff_expr_voom(object, design_diff_obs))
})

# h_diff_expr_deseq2 ----

test_that("h_diff_expr_deseq2 works as expected", {
  object <- hermes_data
  design <- model.matrix(~SEX, colData(object))
  result <- h_diff_expr_deseq2(object, design)
  expect_data_frame(result)
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$adj_p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("h_diff_expr_deseq2 can pass arguments to DESeq2::DESeq", {
  object <- hermes_data
  design <- model.matrix(~SEX, colData(object))
  result <- expect_silent(h_diff_expr_deseq2(
    object,
    design = design,
    # Use all possible DESeq2::DESeq arguments below.
    fitType = "local",
    sfType = "poscounts",
    minReplicatesForReplace = 10,
    useT = TRUE,
    minmu = 0.7
  ))
  expect_data_frame(result)
  result_orig <- h_diff_expr_deseq2(object, design)
  expect_false(identical(rownames(result), rownames(result_orig)))
})

test_that("h_diff_expr_deseq2 fails if design matrix is not correct", {
  object <- hermes_data
  design_too_wide <- model.matrix(~ SEX + COUNTRY, colData(object))
  expect_error(h_diff_expr_voom(object, design_too_wide))
  design_diff_obs <- model.matrix(~SEX, colData(object)[1:10, ])
  expect_error(h_diff_expr_deseq2(object, design_diff_obs))
})

# diff_expression ----

test_that("diff_expression works as expected with the DESeq2 method", {
  object <- hermes_data
  colData(object) <- df_cols_to_factor(colData(object))
  result <- expect_silent(diff_expression(object, group = "SEX", method = "deseq2"))
  expect_s4_class(result, "HermesDataDiffExpr")
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$adj_p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("diff_expression works as expected with the voom method", {
  object <- hermes_data
  colData(object) <- df_cols_to_factor(colData(object))
  result <- expect_silent(diff_expression(object, group = "SEX", method = "voom"))
  expect_s4_class(result, "HermesDataDiffExpr")
  expect_named(result, c("log2_fc", "stat", "p_val", "adj_p_val"))
  expect_true(S4Vectors::isSorted(result$adj_p_val))
  expect_setequal(rownames(object), rownames(result))
})

test_that("diff_expression allows passing of method arguments to helper functions", {
  object <- hermes_data
  colData(object) <- df_cols_to_factor(colData(object))
  expect_silent(diff_expression(object, group = "SEX", method = "voom", robust = TRUE))
  expect_silent(diff_expression(object, group = "SEX", method = "deseq2", fitType = "local"))
})

test_that("diff_expression fails as expected with inappropriate inputs", {
  object <- hermes_data
  gse <- get_se()
  expect_error(diff_expression(object, "COUNRTY", "deseq2"))
  expect_error(diff_expression(object, "COUNRTY", "anbc"))
  expect_error(diff_expression(gse, "SEX", "deseq2"))
})


# autoplot-HermesDataDiffExpr ----

test_that("autoplot for HermesDataDiffExpr works as expected with default options", {
  dat <- hermes_data
  colData(dat) <- df_cols_to_factor(colData(dat))
  object <- diff_expression(dat, "SEX", "voom")
  result <- autoplot(object)

  set.seed(123)
  vdiffr::expect_doppelganger("autoplot for HermesDataDiffExpr with default options", result)
})

test_that("autoplot for HermesDataDiffExpr works as expected with custom options", {
  dat <- hermes_data
  colData(dat) <- df_cols_to_factor(colData(dat))
  object <- diff_expression(dat, "SEX", "voom")
  result <- autoplot(object, adj_p_val_thresh = 0.92, log2_fc_thresh = 3)

  x <- layer_data(result, 1)
  x <- x[!is.na(x$label), ]
  expect_equal(x$x, c(-3.44, -3.19, 3.39, 3.21, 3.8, 3.12, -3.13), tolerance = 1e-2)
  expect_equal(x$y, rep(0.04008, 7), tolerance = 1e-2)
  expect_identical(
    x$label,
    c(
      "GeneID:151242", "GeneID:390084", "GeneID:9834", "GeneID:7980",
      "GeneID:2262", "GeneID:6328", "GeneID:149998"
    )
  )
})
