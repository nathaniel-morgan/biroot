test_that("iso_discrete() four-class case draws a full plus through the center", {
  sq <- data.frame(
    x = c(0, 0, 1, 1),
    y = c(0, 1, 1, 0),
    id = "t",
    value = c("a", "b", "c", "d")
  )
  out <- iso_discrete(sq)
  expect_equal(nrow(out), 6)
  # horizontal arm must reach the right edge midpoint (1, 0.5)
  expect_true(any(abs(out$x - 1) < 1e-8 & abs(out$y - 0.5) < 1e-8))
  # and the left edge, top, and bottom midpoints
  expect_true(any(abs(out$x - 0) < 1e-8 & abs(out$y - 0.5) < 1e-8))
  expect_true(any(abs(out$x - 0.5) < 1e-8 & abs(out$y - 1) < 1e-8))
  expect_true(any(abs(out$x - 0.5) < 1e-8 & abs(out$y - 0) < 1e-8))
})

test_that("four-class squares in biroot_lines() output reach all four edges", {
  # quadrant classifier: the leaf square just below-left of the origin has all
  # four classes at its corners
  f <- function(v) with(v, paste0(x >= 0, "-", y >= 0))
  pts <- biroot(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 4, class = "discrete")
  leaves <- subset(pts, depth == 4)
  four_class_ids <- unlist(lapply(
    split.data.frame(leaves, leaves$id),
    function(s) if (length(unique(s$value)) == 4) s$id[1] else NULL
  ))
  expect_gte(length(four_class_ids), 1)

  out <- biroot_lines(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 4, class = "discrete")
  expect_false(any(out$id == "forgot case"))

  id0 <- four_class_ids[1]
  s <- subset(leaves, id == id0)
  x0 <- min(s$x); x1 <- max(s$x); xm <- (x0 + x1) / 2
  y0 <- min(s$y); y1 <- max(s$y); ym <- (y0 + y1) / 2
  seg <- subset(out, id %in% c(id0, paste0(id0, "-2")))
  expect_equal(nrow(seg), 6)
  expect_true(any(abs(seg$x - x1) < 1e-8 & abs(seg$y - ym) < 1e-8))
  expect_true(any(abs(seg$x - x0) < 1e-8 & abs(seg$y - ym) < 1e-8))
  expect_true(any(abs(seg$y - y1) < 1e-8 & abs(seg$x - xm) < 1e-8))
  expect_true(any(abs(seg$y - y0) < 1e-8 & abs(seg$x - xm) < 1e-8))
})

test_that("three-class boundaries come out clean", {
  f <- function(v) with(v, ifelse(v$y >= 0, "top", ifelse(v$x >= 0, "br", "bl")))
  out <- biroot_lines(f, xlim = c(-1, 1), ylim = c(-1, 1), max_depth = 4, class = "discrete")
  expect_gt(nrow(out), 0)
  expect_false(any(out$id == "forgot case"))
})
