# draw_libsize_hist ----

test_that("draw_libsize_hist works as expected", {
  result <- expect_silent(draw_libsize_hist(hermes_data, bins = 10L, fill = "blue"))

  vdiffr::expect_doppelganger("draw_libsize_hist with 10 blue bins", result)
})

# draw_libsize_qq ----

test_that("draw_libsize_qq works as expected", {
  result <- expect_silent(draw_libsize_qq(hermes_data, color = "blue", linetype = "solid"))

  vdiffr::expect_doppelganger("draw_libsize_qq with blue solid lines", result)
})

# draw_libsize_densities ----

test_that("draw_libsize_densities works as expected", {
  result <- draw_libsize_densities(hermes_data)

  vdiffr::expect_doppelganger("draw_libsize_densities with default log", result)
})

# draw_nonzero_boxplot ----

test_that("draw_nonzero_boxplot works as expected with default options", {
  set.seed(123)
  result <- draw_nonzero_boxplot(hermes_data)

  vdiffr::expect_doppelganger("draw_nonzero_boxplot with default options", result)
})

test_that("draw_nonzero_boxplot works as expected with custom options", {
  result <- draw_nonzero_boxplot(hermes_data, position = position_identity(), alpha = 1)

  vdiffr::expect_doppelganger("draw_nonzero_boxplot with custom options", result)
})

# draw_genes_barplot ----

test_that("draw_genes_barplot works as expected with default options", {
  result <- draw_genes_barplot(hermes_data)

  vdiffr::expect_doppelganger("draw_genes_barplot with default options", result)
})

test_that("draw_genes_barplot works as expected with custom options", {
  result <- draw_genes_barplot(hermes_data, chromosomes = c("3", "11"), include_others = FALSE)

  vdiffr::expect_doppelganger("draw_genes_barplot with custom options", result)
})

# plot_all ----

test_that("autoplot function works as expected on HermesData", {
  object <- hermes_data
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
  expect_error(autoplot(object1))
})
