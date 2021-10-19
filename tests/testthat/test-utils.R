# df_char_to_factor ----

test_that("df_char_to_factor works as expected with default settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA)
  )
  result <- df_char_to_factor(dat)
  expected <- S4Vectors::DataFrame(
    a = factor(c("<Missing>", "B"), levels = c("B", "<Missing>")),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = factor(c("U", "<Missing>"), levels = c("U", "<Missing>"))
  )
  expect_identical(result, expected)
})

test_that("df_char_to_factor works as expected with custom settings", {
  dat <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = c("U", NA)
  )
  result <- df_char_to_factor(dat, omit_columns = c("a", "b"), na_level = "Misses")
  expected <- S4Vectors::DataFrame(
    a = c(NA, "B"),
    b = array(1:4, c(2, 1, 2)),
    c = c(TRUE, FALSE),
    d = factor(c("X", NA)),
    e = factor(c("U", "Misses"), levels = c("U", "Misses"))
  )
  expect_identical(result, expected)
})

# h_short_list ----

test_that("h_short_list works as expected with default arguments", {
  expect_identical(
    h_short_list(letters),
    "a, b, ..., z"
  )
  expect_identical(
    h_short_list(letters[1:3]),
    "a, b, c"
  )
  expect_identical(
    h_short_list(letters[1:2]),
    "a, b"
  )
  expect_identical(
    h_short_list(letters[1]),
    "a"
  )
})

test_that("h_short_list works as expected with custom arguments", {
  expect_identical(
    h_short_list(letters, sep = "; "),
    "a; b; ...; z"
  )
  expect_identical(
    h_short_list(letters[1:4], thresh = 4L),
    "a, b, c, d"
  )
  expect_identical(
    h_short_list(letters[1:5], thresh = 5L, sep = "-"),
    "a-b-c-d-e"
  )
  expect_error(h_short_list(letters[1:3], thresh = 2L))
  expect_error(h_short_list(letters[1:3], sep = ""))
})

# h_parens ----

test_that("h_parens works as expected", {
  expect_identical(
    h_parens("bla"),
    "(bla)"
  )
  expect_identical(
    h_parens(""),
    ""
  )
  expect_error(h_parens(123))
  expect_error(h_parens(NULL))
})

# cat_with_newline ----

test_that("cat_with_newline works as expected", {
  expect_equal(
    capture.output({
      cat_with_newline("hello")
      cat_with_newline("world", append = TRUE)
    }),
    c("hello", "world")
  )

  filename <- tempfile()
  on.exit(unlink(filename))
  cat_with_newline("hello", file = filename)
  cat_with_newline("world", append = TRUE, file = filename)
  expect_equal(readLines(filename), c("hello", "world"))
})
