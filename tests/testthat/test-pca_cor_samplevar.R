# h_pca_var_rsquared ----

test_that("h_pca_var_rsquared works as expected", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  x <- expect_silent(colData(object)$LowDepthFlag)
  r2 <- expect_silent(h_pca_var_rsquared(pca, x))
  expect_is(r2, "numeric")
  expect_true(noNA(r2))
  expect_identical(ncol(pca), length(r2))
})

test_that("h_pca_var_rsquared fails as expected with invalid inputs", {
  se <- get_se()
  object <- HermesData(se)
  x <- expect_silent(colData(se)$LowDepthFlag)
  expect_error(h_pca_var_rsquared(se, x))
  expect_error(h_pca_var_rsquared(object, x))
})

test_that("h_pca_var_rsquared returns NAs when something is not estimable", {
  pca <- rbind(X = -1, Y = 1)
  x <- c(TRUE, FALSE)
  r2x <- expect_silent(h_pca_var_rsquared(pca, x))
  expect_identical(r2x, NA_real_)
  y <- c(FALSE, FALSE)
  r2y <- expect_silent(h_pca_var_rsquared(pca, y))
  expect_identical(r2y, NA_real_)
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
