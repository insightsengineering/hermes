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
