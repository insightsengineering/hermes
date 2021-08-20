# plot_all ----
test_that("autoplot function works as expected on HermesData", {
  test.nest::skip_if_too_deep(0)

  object <- HermesData(summarized_experiment)
  result <- autoplot(object)
  expect_is(result, "list")
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot function works as expected on RangedHermesData", {
  test.nest::skip_if_too_deep(0)

  object <- HermesData(get_rse())
  result <- autoplot(object)
  expect_is(result, "list")
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot fails as expected with invalid objects", {
  test.nest::skip_if_too_deep(0)

  object1 <- get_se()
  expect_error(autoplot(object1))
})
