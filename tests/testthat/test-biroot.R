test_that("max_depth = 1 caps refinement at depth 1 with no duplicate squares", {
  f <- function(v) with(v, x^2 + y^2 - 1)
  out <- biroot(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 1)
  expect_equal(max(out$depth), 1)
  d1 <- subset(out, depth == 1)
  expect_equal(nrow(d1), 16)
  expect_false(any(duplicated(d1[, c("id", "position")])))
})

test_that("max_depth is validated", {
  f <- function(v) with(v, x)
  expect_error(biroot(f, max_depth = 0), "max_depth")
  expect_error(biroot(f, max_depth = c(2, 3)), "max_depth")
})

test_that("a zero-free function stops refining after min_depth + 1", {
  f <- function(v) with(v, x^2 + y^2 + 1)
  expect_no_warning(
    out <- biroot(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 6, min_depth = 2)
  )
  expect_equal(max(out$depth), 3)
})

test_that("exact zeros at corners count as positive for refinement", {
  # f = x has its zero set exactly on grid corners; squares strictly to the
  # right of x = 0 see corner values {0, +} and must not refine past min_depth
  f <- function(v) with(v, x)
  out <- biroot(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 5, min_depth = 2)
  leaves <- subset(out, depth == 5)
  expect_true(nrow(leaves) > 0)
  expect_true(all(leaves$x <= 0 + 1e-8))
})
