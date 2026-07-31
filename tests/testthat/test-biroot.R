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

test_that("point-wise and data-frame f give identical results", {
  f_pt <- function(u) u[1]^2 + u[2]^2 - 1
  f_df <- function(v) with(v, x^2 + y^2 - 1)
  b_pt <- biroot(f_pt, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 5)
  b_df <- biroot(f_df, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 5)
  expect_identical(b_pt, b_df)
  expect_identical(biroot_paths(b_pt), biroot_paths(b_df))
})

test_that("point-wise discrete classifiers are accepted", {
  f_pt <- function(u) if (u[1] >= 0) "right" else "left"
  b <- biroot(f_pt, max_depth = 4, class = "discrete")
  expect_setequal(unique(b$value), c("left", "right"))
  d <- biroot_discrete(f_pt, max_depth = 3)
  expect_setequal(unique(d$class), c("left", "right"))
})

test_that("an unusable f errors with the contract and the underlying message", {
  expect_error(biroot(function(v) c(1, 2), max_depth = 3), "single value")
  expect_error(biroot(function(v) stop("boom"), max_depth = 3), "boom")
  expect_error(biroot(function(v) data.frame(a = 1), max_depth = 3), "single value")
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
