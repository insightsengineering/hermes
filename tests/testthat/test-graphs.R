# plot_all ----
test_that("autoplot function works as expected on HermesData", {
  object <- HermesData(summarized_experiment)
  result <- autoplot(object)
  expect_is(result, "list")
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot function works as expected on RangedHermesData", {
  object <- HermesData(get_rse())
  result <- autoplot(object)
  expect_is(result, "list")
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot fails as expected with invalid objects", {
  object1 <- get_se()
  object2 <- matrix(1:4, 2, 2)
  expect_error(autoplot(object1))
  expect_error(autoplot(object2))
})