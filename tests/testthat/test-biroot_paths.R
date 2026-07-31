test_that("the circle stitches into one closed, ordered, interpolated chain", {
  f <- function(v) with(v, x^2 + y^2 - 1)
  p <- biroot_paths(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6)
  expect_equal(length(unique(p$id)), 1)
  expect_true(all(p$closed))
  expect_false(any(p$crack))
  # closed chains repeat their first vertex bitwise as the last row
  expect_identical(c(p$x[1], p$y[1]), c(p$x[nrow(p)], p$y[nrow(p)]))
  # ordered: consecutive vertices no farther apart than a cell diagonal
  cell <- 4 / 2^6
  d <- sqrt(diff(p$x)^2 + diff(p$y)^2)
  expect_true(all(d <= sqrt(2) * cell + 1e-12))
  expect_true(all(d > 0))
  # interpolation puts vertices much closer to the circle than the cell size
  expect_true(all(abs(sqrt(p$x^2 + p$y^2) - 1) < cell / 10))
})

test_that("function and data-frame inputs give identical() results", {
  f <- function(v) with(v, x^2 + y^2 - 1)
  b <- biroot(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6)
  expect_identical(
    biroot_paths(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6),
    biroot_paths(b)
  )
})

test_that("open chains end on the window border with correct side labels", {
  p <- biroot_paths(function(v) with(v, y - x), max_depth = 5)
  expect_equal(length(unique(p$id)), 1)
  expect_false(any(p$closed))
  on_border <- function(x, y) x %in% c(-1, 1) | y %in% c(-1, 1)
  expect_true(on_border(p$x[1], p$y[1]))
  expect_true(on_border(p$x[nrow(p)], p$y[nrow(p)]))
  expect_setequal(c(p$left[1], p$right[1]), c(-1L, 1L))
})

test_that("a measure-zero boundary is kept in paths with equal side labels", {
  p <- biroot_paths(function(v) with(v, -x^2), max_depth = 5)
  expect_gt(nrow(p), 0)
  expect_true(all(p$left == -1L))
  expect_true(all(p$right == -1L))
  expect_true(all(p$x == 0))
})

test_that("zero-free functions give a typed empty paths result", {
  p <- biroot_paths(function(v) with(v, x^2 + y^2 + 1), max_depth = 5)
  expect_equal(nrow(p), 0)
  expect_type(p$x, "double")
  expect_type(p$id, "integer")
  expect_type(p$left, "integer")
  expect_type(p$crack, "logical")
})

test_that("data-frame input rejects explicit geometry arguments", {
  b <- biroot(function(v) with(v, x), max_depth = 3)
  expect_error(biroot_paths(b, xlim = c(0, 1)), "must not be supplied")
  expect_error(biroot_paths(b, max_depth = 5), "must not be supplied")
})

test_that("biroot_discrete() output is rejected with guidance", {
  d <- biroot_discrete(function(v) ifelse(v$x >= 0, "a", "b"), max_depth = 3)
  expect_error(biroot_paths(d), "biroot_discrete")
})

test_that("a character value column contradicting class = continuous errors", {
  b <- biroot(function(v) ifelse(v$x >= 0, "a", "b"), max_depth = 3, class = "discrete")
  expect_error(biroot_paths(b, class = "continuous"), "contradicts")
})

test_that("discrete boundaries stitch through junctions", {
  f <- function(v) with(v, paste0(x >= 0, "-", y >= 0))
  p <- biroot_paths(f, max_depth = 4, class = "discrete")
  expect_gt(nrow(p), 0)
  expect_false(any(p$crack))
  # the four arms meet at the cell-center junction (-h/2, -h/2)
  h <- 2 / 2^4
  expect_true(any(p$x == -h / 2 & p$y == -h / 2))
  # each chain separates two distinct classes
  expect_true(all(p$left != p$right))
})
