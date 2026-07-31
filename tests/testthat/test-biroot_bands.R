shoelace_df <- function(d) {
  n <- nrow(d); j <- c(2:n, 1L)
  sum(d$x * d$y[j] - d$x[j] * d$y) / 2
}

ring_areas <- function(b) {
  vapply(split(b, paste(b$id, b$subid)), shoelace_df, numeric(1))
}

face_area <- function(b, i) sum(ring_areas(b[b$id == i, ]))

test_that("signed ring areas partition the window exactly", {
  cases <- list(
    list(f = function(v) with(v, x^2 + y^2 - 1), class = "continuous"),
    list(f = function(v) with(v, x - y), class = "continuous"),
    list(f = function(v) with(v, paste0(x >= 0, "-", y >= 0)), class = "discrete")
  )
  for (cs in cases) {
    b <- biroot_bands(cs$f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 5,
                      class = cs$class)
    expect_equal(sum(ring_areas(b)), 16, tolerance = 1e-12)
  }
})

test_that("annulus nesting: island is its own face, hole belongs to the annulus", {
  f <- function(v) with(v, (x^2 + y^2 - 1) * (x^2 + y^2 - 0.25))
  b <- biroot_bands(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 7)
  expect_equal(length(unique(b$id)), 3)
  expect_equal(sum(ring_areas(b)), 16, tolerance = 1e-12)

  areas <- vapply(unique(b$id), function(i) face_area(b, i), numeric(1))
  vals <- vapply(unique(b$id), function(i) b$value[b$id == i][1], integer(1))
  cell <- 4 / 2^7
  # negative annulus: two rings (outer CCW, hole CW), area ~ 3pi/4
  ann <- unique(b$id)[vals == -1L]
  expect_length(ann, 1)
  ra <- ring_areas(b[b$id == ann, ])
  expect_length(ra, 2)
  expect_true(any(ra > 0) && any(ra < 0))
  expect_equal(areas[match(ann, unique(b$id))], 3 * pi / 4, tolerance = 8 * cell)
  # positive faces: inner disk ~ pi/4 and window remainder ~ 16 - pi
  pos <- sort(areas[vals == 1L])
  expect_equal(pos[1], pi / 4, tolerance = 8 * cell)
  expect_equal(pos[2], 16 - pi, tolerance = 8 * cell)
})

test_that("exact dyadic cases give exact areas", {
  # diagonal through window corners
  b <- biroot_bands(function(v) with(v, x - y), max_depth = 5)
  expect_equal(length(unique(b$id)), 2)
  expect_identical(sort(vapply(unique(b$id), function(i) face_area(b, i), numeric(1))),
                   c(2, 2))
  # zero set exactly on grid corners
  b <- biroot_bands(function(v) with(v, x), max_depth = 5)
  expect_identical(sort(vapply(unique(b$id), function(i) face_area(b, i), numeric(1))),
                   c(2, 2))
  # degree-4 pinch at the origin: four quadrant wedges of area 1
  b <- biroot_bands(function(v) with(v, x^2 - y^2), max_depth = 5)
  expect_equal(length(unique(b$id)), 4)
  expect_identical(vapply(unique(b$id), function(i) face_area(b, i), numeric(1)),
                   c(1, 1, 1, 1))
})

test_that("measure-zero boundaries produce no extra faces", {
  b <- biroot_bands(function(v) with(v, -x^2), max_depth = 5)
  expect_equal(length(unique(b$id)), 1)
  expect_identical(b$value[1], -1L)
  expect_equal(sum(ring_areas(b)), 4, tolerance = 1e-12)
})

test_that("zero-free functions give the whole window as one face", {
  b <- biroot_bands(function(v) with(v, x^2 + y^2 + 1), max_depth = 5)
  expect_equal(length(unique(b$id)), 1)
  expect_equal(length(unique(b$subid)), 1)
  expect_identical(b$value[1], 1L)
  expect_equal(sum(ring_areas(b)), 4, tolerance = 1e-12)
})

test_that("a fully interior contour still closes against the window ring", {
  b <- biroot_bands(function(v) with(v, x^2 + y^2 - 0.25), max_depth = 6)
  expect_equal(length(unique(b$id)), 2)
  outer_face <- unique(b$id)[vapply(unique(b$id), function(i) b$value[b$id == i][1], integer(1)) == 1L]
  expect_equal(length(unique(b$subid[b$id == outer_face])), 2)
  expect_equal(sum(ring_areas(b)), 4, tolerance = 1e-12)
})

test_that("discrete quadrant regions have closed-form midpoint areas", {
  f <- function(v) with(v, paste0(x >= 0, "-", y >= 0))
  h <- 2 / 2^4
  b <- biroot_bands(f, max_depth = 4, class = "discrete")
  expect_equal(length(unique(b$id)), 4)
  areas <- vapply(unique(b$id), function(i) face_area(b, i), numeric(1))
  vals <- vapply(unique(b$id), function(i) b$value[b$id == i][1], character(1))
  expect_equal(areas[vals == "TRUE-TRUE"], (1 + h / 2)^2)
  expect_equal(areas[vals == "FALSE-FALSE"], (1 - h / 2)^2)
  expect_equal(areas[vals == "FALSE-TRUE"], (1 - h / 2) * (1 + h / 2))
  expect_equal(areas[vals == "TRUE-FALSE"], (1 - h / 2) * (1 + h / 2))
})

test_that("three-class junction regions assemble with exact areas", {
  f <- function(v) ifelse(v$y >= 0, "top", ifelse(v$x >= 0, "br", "bl"))
  h <- 2 / 2^4
  b <- biroot_bands(f, max_depth = 4, class = "discrete")
  expect_equal(length(unique(b$id)), 3)
  areas <- vapply(unique(b$id), function(i) face_area(b, i), numeric(1))
  vals <- vapply(unique(b$id), function(i) b$value[b$id == i][1], character(1))
  expect_equal(areas[vals == "top"], 2 * (1 + h / 2))
  expect_equal(areas[vals == "bl"], (1 - h / 2)^2)
  expect_equal(areas[vals == "br"], (1 + h / 2) * (1 - h / 2))
})

test_that("class is recovered from the biroot() attribute", {
  f <- function(v) with(v, paste0(x >= 0, "-", y >= 0))
  bd <- biroot(f, max_depth = 4, class = "discrete")
  b <- biroot_bands(bd)
  expect_equal(length(unique(b$id)), 4)
})

test_that("outer rings are counterclockwise, holes clockwise", {
  f <- function(v) with(v, x^2 + y^2 - 1)
  b <- biroot_bands(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6)
  for (i in unique(b$id)) {
    bi <- b[b$id == i, ]
    for (j in unique(bi$subid)) {
      a <- shoelace_df(bi[bi$subid == j, ])
      if (j == 1L) expect_gt(a, 0) else expect_lt(a, 0)
    }
  }
})

test_that("the documented ggplot calls build", {
  skip_if_not_installed("ggplot2")
  f <- function(v) with(v, x^2 + y^2 - 1)
  b <- biroot_bands(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 5)
  p <- biroot_paths(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 5)
  g <- ggplot2::ggplot(b, ggplot2::aes(x, y, group = id, subgroup = subid)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = factor(value)), color = NA) +
    ggplot2::geom_path(data = p, ggplot2::aes(x, y, group = id),
                       inherit.aes = FALSE)
  expect_no_error(ggplot2::ggplot_build(g))
})
