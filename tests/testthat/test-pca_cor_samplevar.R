# h_pca_df_r2_matrix ----

test_that("h_pca_var_rsquared works as expected for HermesData", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  x <- expect_silent(colData(object)$LowDepthFlag)
  r2 <- expect_silent(h_pca_var_rsquared(pca, x))
  expect_is(r2, "numeric")
  expect_identical(ncol(pca), length(r2))
})

test_that("h_pca_var_rsquared works as expected for RangedHermesData", {
  object <- HermesData(get_rse())
  pca <- expect_silent(calc_pca(object)$x)
  x <- expect_silent(colData(object)$LowDepthFlag)
  r2 <- expect_silent(h_pca_var_rsquared(pca, x))
  expect_is(r2, "numeric")
  expect_identical(ncol(pca), length(r2))
})

test_that("h_pca_var_rsquared fails as expected with invalid settings", {
  se <- get_se()
  object <- HermesData(se)
  x <- expect_silent(colData(se)$LowDepthFlag)
  expect_error(h_pca_var_rsquared(se, x))
  expect_error(h_pca_var_rsquared(object, x))
})

# h_pca_df_r2_matrix ----

test_that("h_pca_df_r2_matrix works as expected", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  result <- expect_silent(h_pca_df_r2_matrix(pca, df))
  expect_is(result, "matrix")
  expect_identical(nrow(result), ncol(pca))
  expect_lt(ncol(result), ncol(df))
  expect_true(all(colnames(result) %in% colnames(df)))
})

test_that("h_pca_df_r2_matrix fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  expect_error(h_pca_df_r2_matrix(pca, object))
  expect_error(h_pca_var_rsquared(object, df))
})

# correlate-HermesDataPca ----

test_that("correlate method on HermesDataPca works as expected", {
  object <- HermesData(summarized_experiment)
  df <- expect_silent(as.data.frame(colData(object)))
  pca <- expect_silent(calc_pca(object))
  result <- expect_silent(correlate(pca, object))
  is(result, "HermesDataPcaCor")
  expect_identical(nrow(result), ncol(pca$x))
  expect_lt(ncol(result), ncol(df))
  expect_identical(rownames(result), colnames(pca$x))
  expect_true(all(colnames(result) %in% colnames(df)))
})

test_that("correlate method fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object))
  result <- expect_silent(correlate(pca, object))
  expect_error(correlate(pca, colData(object)))
  expect_error(correlate(pca$x, object))
})
