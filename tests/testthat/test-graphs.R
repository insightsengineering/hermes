# draw_libsize_hist ----

test_that("draw_libsize_hist works as expected", {
  result <- expect_silent(draw_libsize_hist(hermes_data, bins = 10L, fill = "blue"))

  expect_snapshot(ggplot2::layer_data(result))
})

# draw_libsize_qq ----

test_that("draw_libsize_qq works as expected", {
  result <- expect_silent(draw_libsize_qq(hermes_data, color = "blue", linetype = "solid"))

  expect_snapshot(ggplot2::layer_data(result))
})

# draw_libsize_densities ----

test_that("draw_libsize_densities works as expected", {
  set.seed(451)
  result <- draw_libsize_densities(hermes_data)

  result <- ggplot2::layer_data(result) %>%
    mutate(across(where(is.numeric), function(x) round(x, 2)))

  expect_snapshot(result)
})

# draw_nonzero_boxplot ----

test_that("draw_nonzero_boxplot works as expected with default options", {
  set.seed(123)
  result <- draw_nonzero_boxplot(hermes_data)

  expect_snapshot(ggplot2::layer_data(result))
  expect_snapshot(ggplot2::layer_data(result, 2))
  expect_snapshot(ggplot2::layer_data(result, 3))
})

test_that("draw_nonzero_boxplot works as expected with custom options", {
  result <- draw_nonzero_boxplot(hermes_data, position = position_identity(), alpha = 1)

  expect_snapshot(ggplot2::layer_data(result))
  expect_snapshot(ggplot2::layer_data(result, 2))
  expect_snapshot(ggplot2::layer_data(result, 3))
})

# draw_genes_barplot ----

test_that("draw_genes_barplot works as expected with default options", {
  result <- draw_genes_barplot(hermes_data)

  expect_snapshot(ggplot2::layer_data(result))
})

test_that("draw_genes_barplot works as expected with custom options", {
  result <- draw_genes_barplot(hermes_data, chromosomes = c("3", "11"), include_others = FALSE)

  expect_snapshot(ggplot2::layer_data(result))
})

# plot_all ----

test_that("autoplot function works as expected on HermesData", {
  object <- hermes_data
  result <- autoplot(object)
  expect_list(result, types = "ggplot", len = 5)
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot function works as expected on RangedHermesData", {
  object <- HermesData(get_rse())
  result <- autoplot(object)
  expect_list(result, types = "ggplot", len = 5)
  expect_named(result, c("libsize_hist", "libsize_qq", "libsize_densities", "nonzero_boxplot", "genes_barplot"))
})

test_that("autoplot fails as expected with invalid objects", {
  object1 <- get_se()
  expect_error(autoplot(object1))
})
