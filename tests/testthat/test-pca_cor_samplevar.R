# h_pca_df_r2_matrix() tests ----

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

# h_pca_df_r2_matrix tests ----

test_that("h_pca_df_r2_matrix works as expected", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  x <- expect_silent(colData(object)$LowDepthFlag)
  r2 <- expect_silent(h_pca_var_rsquared(pca, x))
  
  r2_all <- expect_silent(h_pca_df_r2_matrix(pca, df))
  
  is_accepted_type <- vapply(df, function(x) {
    is.numeric(x) || is.character(x) || is.factor(x) || is.logical(x)
  }, TRUE)
  df1 <- df[, is_accepted_type]
  is_all_na <- vapply(df1, all_na, TRUE)
  df1 <- df1[, !is_all_na]
  is_all_constant <- vapply(df1, is_constant, TRUE)
  df1 <- df1[, !is_all_constant]
  too_many_levels <- vapply(df1, function(x) {
    (is.character(x) || is.factor(x)) && (length(unique(x)) > nrow(df1)/2)
  }, TRUE)
  df1 <- df1[, !too_many_levels]
  
  expect_is(r2_all, "matrix")
  expect_identical(nrow(r2_all), length(r2))
  expect_identical(ncol(r2_all), ncol(df1))
})

test_that("h_pca_df_r2_matrix fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  x <- expect_silent(colData(object)$LowDepthFlag)
  
  expect_error(h_pca_df_r2_matrix(pca, object))
  expect_error(h_pca_var_rsquared(object, df))
})

# correlate tests ----
test_that("correlate method works as expected", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  r2_all <- expect_silent(h_pca_df_r2_matrix(pca, df))
  
  object_pca <- expect_silent(calc_pca(object))
  result <- expect_silent(correlate(object_pca, object))
  
  is(result, "HermesDataPcaCor")
  expect_identical(nrow(r2_all), nrow(result))
  expect_identical(ncol(r2_all), ncol(result))
  expect_identical(rownames(r2_all), rownames(result))
  expect_identical(colnames(r2_all), colnames(result))
})

test_that("correlate method fails as expected with invalid settings", {
  object <- HermesData(summarized_experiment)
  pca <- expect_silent(calc_pca(object)$x)
  df <- expect_silent(as.data.frame(colData(object)))
  r2_all <- expect_silent(h_pca_df_r2_matrix(pca, df))
  
  pca_result <- expect_silent(calc_pca(object))
  result <- expect_silent(correlate(pca_result, object))
  
  expect_error(correlate(pca_result, df))
  ### expect_error(correlate(pca, object)) ### DOES NOT GIVE AN ERROR ###
})

