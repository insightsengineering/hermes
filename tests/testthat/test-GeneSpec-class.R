# GeneSpec ----

test_that("GeneSpec initialization works as expected", {
  spec <- expect_silent(GeneSpec$new("GeneID:1820"))
  expect_r6(spec, "GeneSpec")
  expect_identical(spec$get_genes(), "GeneID:1820")

  spec2 <- expect_silent(GeneSpec$new(c("a", "b"), fun = paste))
  expect_r6(spec2, "GeneSpec")
  expect_identical(spec2$get_genes(), c("a", "b"))

  spec3 <- expect_silent(GeneSpec$new())
  expect_r6(spec3, "GeneSpec")
  expect_identical(spec3$get_genes(), NULL)
})

test_that("GeneSpec returns_vector method works as expected", {
  spec <- expect_silent(GeneSpec$new("GeneID:1820", fun = colMeans))
  expect_identical(spec$returns_vector(), TRUE)

  spec2 <- expect_silent(GeneSpec$new(c("a", "b")))
  expect_identical(spec2$returns_vector(), FALSE)

  spec3 <- expect_silent(GeneSpec$new(c("a", "b"), fun = colMeans))
  expect_identical(spec3$returns_vector(), TRUE)
})

test_that("GeneSpec get_label method works as expected", {
  spec <- expect_silent(GeneSpec$new("GeneID:1820", fun = colMeans))
  expect_identical(spec$get_label(), "GeneID:1820")

  spec2 <- expect_silent(GeneSpec$new(c("a", "b"), fun = colMeans))
  expect_identical(spec2$get_label(), "colMeans(a, b)")

  spec3 <- expect_silent(GeneSpec$new(letters, fun = colMeans, fun_name = "avg"))
  expect_identical(spec3$get_label(), "avg(a, b, ..., z)")
})

test_that("GeneSpec extract method works as expected", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), NULL)
  )

  spec <- expect_silent(GeneSpec$new("a", fun = colMeans))
  expect_identical(
    spec$extract(mat),
    mat[1L, ]
  )

  spec2 <- expect_silent(GeneSpec$new(c("a", "b"), fun = colMeans))
  expect_identical(
    spec2$extract(mat),
    colMeans(mat[1:2, ])
  )

  spec3 <- expect_silent(GeneSpec$new(c("a", "b")))
  expect_identical(
    spec3$extract(mat),
    mat[1:2, ]
  )

  spec4 <- expect_silent(GeneSpec$new())
  expect_identical(
    spec4$extract(mat),
    mat[NULL, ]
  )

  expect_error(
    spec4$extract(as.data.frame(mat)),
    "Must inherit from class 'matrix'"
  )

  mat2 <- mat
  rownames(mat2) <- letters[4:6]
  expect_error(
    spec3$extract(mat2),
    "Must include the elements {a,b}",
    fixed = TRUE
  )
})

# gene_spec ----

test_that("gene_spec constructor works as expected", {
  expect_equal(
    GeneSpec$new("GeneID:1820"),
    gene_spec("GeneID:1820")
  )
})
