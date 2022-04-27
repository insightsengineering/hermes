# h_unique_labels ----

test_that("h_unique_labels returns NULL if ids is NULL", {
  expect_null(h_unique_labels(NULL))
})

test_that("h_unique_labels returns ids if nms are NULL", {
  result <- h_unique_labels(c("1", "2", "3", "4", "5"))
  expected <- c("1", "2", "3", "4", "5")
  expect_identical(result, expected)
})

test_that("h_unique_labels just returns names if they are already unique", {
  result <- h_unique_labels(
    ids = c("1", "2", "3", "4", "5"),
    nms = c("a", "b", "c", "d", "e")
  )
  expected <- c("a", "b", "c", "d", "e")
  expect_identical(result, expected)
})

test_that("h_unique_labels ensures uniqueness by appending ids in parentheses", {
  result <- h_unique_labels(
    ids = c("1", "2", "3", "4", "5"),
    nms = c("a", "b", "b", "a", "c")
  )
  expected <- c("a (1)", "b (2)", "b (3)", "a (4)", "c")
  expect_identical(result, expected)
})

test_that("h_unique_labels replaces empty names by ids", {
  result <- h_unique_labels(
    ids = c("1", "2", "3", "4", "5"),
    nms = c("a", "", "c", "", "")
  )
  expected <- c("a", "2", "c", "4", "5")
  expect_identical(result, expected)
})

test_that("h_unique_labels replaces empty names by ids and afterwards ensures uniqueness", {
  result <- h_unique_labels(
    ids = c("1", "2", "3", "4", "5"),
    nms = c("a", "", "c", "", "4")
  )
  expected <- c("a", "2", "c", "4 (4)", "4 (5)")
  expect_identical(result, expected)
})

# GeneSpec ----

# $new() ----

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

test_that("GeneSpec initialization also works with duplicate labels", {
  spec <- expect_silent(GeneSpec$new(c(A = "GeneID:1820", A = "GeneID:1821")))
  expect_r6(spec, "GeneSpec")
  expect_identical(spec$get_genes(), c(A = "GeneID:1820", A = "GeneID:1821"))
  expect_identical(spec$get_gene_labels(), c("A (GeneID:1820)", "A (GeneID:1821)"))
})

# $returns_vector() ----

test_that("GeneSpec returns_vector method works as expected", {
  spec <- expect_silent(GeneSpec$new("GeneID:1820", fun = colMeans))
  expect_identical(spec$returns_vector(), TRUE)

  spec2 <- expect_silent(GeneSpec$new(c("a", "b")))
  expect_identical(spec2$returns_vector(), FALSE)

  spec3 <- expect_silent(GeneSpec$new(c("a", "b"), fun = colMeans))
  expect_identical(spec3$returns_vector(), TRUE)
})

# $get_gene_labels() ----

test_that("GeneSpec get_gene_labels method works as expected", {
  spec <- expect_silent(GeneSpec$new(c(a = "123", "435", c = "4353"), fun = colMeans))
  expect_identical(spec$get_gene_labels(), c("a", "435", "c"))
  expect_identical(spec$get_gene_labels("123"), "a")

  spec2 <- expect_silent(GeneSpec$new(c("a", "b")))
  expect_identical(spec2$get_gene_labels(), c("a", "b"))
  expect_error(spec2$get_gene_labels("c"))

  spec3 <- expect_silent(GeneSpec$new(fun = colMeans))
  expect_identical(spec3$get_gene_labels(), NULL)
})

# $get_label() ----

test_that("GeneSpec get_label method works as expected", {
  spec <- expect_silent(GeneSpec$new(c(A = "GeneID:1820"), fun = colMeans))
  expect_identical(spec$get_label(), "A")

  spec2 <- expect_silent(GeneSpec$new(c("a", c = "b"), fun = colMeans))
  expect_identical(spec2$get_label(), "colMeans(a, c)")

  spec3 <- expect_silent(GeneSpec$new(letters, fun = colMeans, fun_name = "avg"))
  expect_identical(spec3$get_label(), "avg(a, b, ..., z)")

  spec4 <- expect_silent(GeneSpec$new(letters))
  expect_identical(spec4$get_label(letters[1:3]), "(a, b, c)")
})

# $extract() ----

test_that("GeneSpec extract method works as expected", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), NULL)
  )

  spec <- expect_silent(GeneSpec$new(c(A = "a"), fun = colMeans))
  result <- spec$extract(mat)
  expected <- c(1, 4, 7, 10, 13)
  expect_identical(result, expected)
  expect_true(spec$returns_vector())

  spec2 <- expect_silent(GeneSpec$new(c("a", D = "b"), fun = colMeans))
  result <- spec2$extract(mat)
  expected <- colMeans(mat[1:2, ])
  expect_identical(result, expected)
  expect_true(spec2$returns_vector())

  spec3 <- expect_silent(GeneSpec$new(c("a", E = "b")))
  result <- spec3$extract(mat)
  expected <- mat[1:2, ]
  rownames(expected) <- c("a", "E")
  expect_identical(result, expected)
  expect_false(spec3$returns_vector())

  spec4 <- expect_silent(GeneSpec$new())
  expect_identical(
    spec4$extract(mat),
    mat[NULL, ]
  )
  expect_false(spec4$returns_vector())

  expect_error(
    spec4$extract(as.data.frame(mat)),
    "Must inherit from class 'matrix'"
  )

  mat2 <- mat
  rownames(mat2) <- letters[4:6]
  expect_error(
    spec3$extract(mat2),
    "include the elements \\{'a','b'\\}"
  )
})

test_that("GeneSpec extract method ensures correct names for result", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), 1:5)
  )

  spec <- expect_silent(GeneSpec$new(c(A = "a", B = "b"), fun = colMedians))
  result <- spec$extract(mat)
  expect_named(result, colnames(mat))

  spec <- expect_silent(GeneSpec$new(c(A = "a")))
  result <- spec$extract(mat)
  expect_named(result, colnames(mat))

  spec <- expect_silent(GeneSpec$new(c(B = "b", A = "a")))
  result <- spec$extract(mat)
  expect_names(rownames(result), identical.to = c("B", "A"))
  expect_names(colnames(result), identical.to = colnames(mat))
})

test_that("GeneSpec extract method also works with duplicate labels", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), 1:5)
  )
  spec <- expect_silent(GeneSpec$new(c(A = "a", A = "b")))
  result <- spec$extract(mat)
  expect_names(rownames(result), identical.to = c("A (a)", "A (b)"))
})

# $extract_data_frame() ----

test_that("GeneSpec extract_data_frame method works as expected", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), NULL)
  )

  spec <- expect_silent(GeneSpec$new(c(A = "a"), fun = colMeans))
  expect_identical(
    spec$extract_data_frame(mat),
    data.frame(A = as.numeric(mat[1L, ]))
  )

  spec2 <- expect_silent(GeneSpec$new(c("a", D = "b"), fun = colMeans))
  expect_identical(
    spec2$extract_data_frame(mat),
    data.frame(`colMeans.a..D.` = colMeans(mat[1:2, ]))
  )

  spec3 <- expect_silent(GeneSpec$new(c("a", "b")))
  expect_identical(
    spec3$extract_data_frame(mat),
    as.data.frame(t(mat[1:2, ]))
  )

  spec4 <- expect_silent(GeneSpec$new())
  expect_equivalent(
    spec4$extract_data_frame(mat),
    as.data.frame(t(mat[NULL, ]))
  )

  expect_error(
    spec4$extract_data_frame(as.data.frame(mat)),
    "Must inherit from class 'matrix'"
  )

  mat2 <- mat
  rownames(mat2) <- letters[4:6]
  expect_error(
    spec3$extract_data_frame(mat2),
    "include the elements \\{'a','b'\\}"
  )
})

test_that("GeneSpec extract_data_frame method ensures correct names for result", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), 5:1)
  )

  spec <- expect_silent(GeneSpec$new(c(A = "a"), fun = colMedians))
  result <- spec$extract_data_frame(mat)
  expect_names(colnames(result), identical.to = "A")
  expect_names(rownames(result), identical.to = colnames(mat))

  spec <- expect_silent(GeneSpec$new(c("c", A = "a")))
  result <- spec$extract_data_frame(mat)
  expect_names(colnames(result), identical.to = c("c", "A"))
  expect_names(rownames(result), identical.to = colnames(mat))

  spec <- expect_silent(GeneSpec$new("c"))
  result <- spec$extract_data_frame(mat)
  expect_names(colnames(result), identical.to = "c")
  expect_names(rownames(result), identical.to = colnames(mat))
})

test_that("GeneSpec extract_data_frame method also works with duplicate labels", {
  mat <- matrix(
    data = 1:15,
    nrow = 3, ncol = 5,
    dimnames = list(c("a", "b", "c"), 1:5)
  )
  spec <- expect_silent(GeneSpec$new(c(A = "a", A = "b")))
  result <- spec$extract_data_frame(mat)
  expect_names(colnames(result), identical.to = c("A..a.", "A..b."))
  # Note that these column names are produced by `make.names()` implicitly.
})

# gene_spec ----

test_that("gene_spec constructor works as expected", {
  expect_equal(
    GeneSpec$new("GeneID:1820"),
    gene_spec("GeneID:1820")
  )
  expect_equal(
    GeneSpec$new(c(D = "GeneID:1820")),
    gene_spec(c(D = "GeneID:1820"))
  )
})
