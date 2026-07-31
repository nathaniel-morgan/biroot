test_that("contours lying exactly on grid corners are not dropped", {
  # the zero set of f = x falls exactly on grid corners at every depth
  f <- function(v) with(v, x)
  out <- biroot_lines(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 5)
  expect_gt(nrow(out), 0)
  cell <- 2 / 2^5
  expect_true(all(abs(out$x) <= cell + 1e-8))
})

test_that("a function with no zeros gives a typed zero-row result", {
  f <- function(v) with(v, x^2 + y^2 + 1)
  out <- biroot_lines(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 6)
  expect_equal(nrow(out), 0)
  expect_type(out$x, "double")
  expect_type(out$y, "double")
  expect_type(out$id, "character")
})

test_that("uniform squares forced to max_depth still give numeric columns", {
  f <- function(v) with(v, x^2 + y^2 + 1)
  out <- biroot_lines(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 3, min_depth = 3)
  expect_equal(nrow(out), 0)
  expect_type(out$x, "double")
  expect_type(out$y, "double")
})

test_that("circle contour endpoints lie near radius 1", {
  f <- function(v) with(v, x^2 + y^2 - 1)
  out <- biroot_lines(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 7)
  expect_gt(nrow(out), 0)
  r <- with(out, sqrt(x^2 + y^2))
  cell <- 4 / 2^7
  expect_true(all(abs(r - 1) < 2 * cell))
})

test_that("iso() applies the zero-counts-as-positive convention", {
  sq <- data.frame(
    x = c(0, 0, 1, 1),
    y = c(0, 1, 1, 0),
    id = "t",
    value = c(0, 0, 1, 1)
  )
  # an all-nonnegative square is uniform: NA sentinel row, numeric columns
  out <- iso(sq)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$x))
  expect_type(out$x, "double")

  # zeros against negatives are a genuine sign change
  sq$value <- c(-1, 0, 0, -1)
  out <- iso(sq)
  expect_equal(nrow(out), 2)
  expect_equal(out$y, c(0.5, 0.5))

  sq$value <- c(0, -1, -1, -1)
  out <- iso(sq)
  expect_equal(nrow(out), 2)
})
