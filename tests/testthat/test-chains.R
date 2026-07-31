# unit tests of the labeled segment emitter against the marching-squares
# topology oracles iso() and iso_discrete()

# a unit cell in the package's bl, tl, tr, br corner order
unit_prep <- function(labels, class = "continuous") {
  if (class == "continuous") {
    list(class = class, xlim = c(0, 1), ylim = c(0, 1),
         X = matrix(c(0, 0, 1, 1), 1), Y = matrix(c(0, 1, 1, 0), 1),
         L = matrix(as.integer(labels), 1), V = matrix(as.numeric(labels), 1))
  } else {
    list(class = class, xlim = c(0, 1), ylim = c(0, 1),
         X = matrix(c(0, 0, 1, 1), 1), Y = matrix(c(0, 1, 1, 0), 1),
         L = matrix(as.character(labels), 1), V = NULL)
  }
}

seg_keys <- function(x0, y0, x1, y1) {
  k0 <- sprintf("%.17g %.17g", x0, y0)
  k1 <- sprintf("%.17g %.17g", x1, y1)
  sort(paste(pmin(k0, k1), pmax(k0, k1)))
}

# decompose an iso()/iso_discrete() result (grouped polylines) into
# undirected edge keys
oracle_keys <- function(df) {
  df <- df[!is.na(df$x), , drop = FALSE]
  if (nrow(df) == 0L) return(character(0))
  out <- character(0)
  for (g in split(df, df$id)) {
    n <- nrow(g)
    if (n < 2L) next
    out <- c(out, seg_keys(g$x[-n], g$y[-n], g$x[-1], g$y[-1]))
  }
  sort(out)
}

test_that("continuous cell segments match iso() topology in all 16 cases", {
  pats <- expand.grid(bl = c(-1, 1), tl = c(-1, 1), tr = c(-1, 1), br = c(-1, 1))
  for (r in seq_len(nrow(pats))) {
    l <- unlist(pats[r, ])
    # with corner values exactly +/-1, interpolated crossings sit at edge
    # midpoints, directly comparable to iso()'s
    segs <- cell_segments(unit_prep(l))
    oracle <- iso(data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0),
                             id = "t", value = l))
    expect_identical(
      seg_keys(segs$x0, segs$y0, segs$x1, segs$y1),
      oracle_keys(oracle),
      info = paste("pattern", paste(l, collapse = ","))
    )
  }
})

test_that("continuous segment side labels are geometrically correct", {
  pats <- expand.grid(bl = c(-1, 1), tl = c(-1, 1), tr = c(-1, 1), br = c(-1, 1))
  cx <- c(0, 0, 1, 1); cy <- c(0, 1, 1, 0)
  for (r in seq_len(nrow(pats))) {
    l <- unlist(pats[r, ])
    segs <- cell_segments(unit_prep(l))
    for (s in seq_len(nrow(segs))) {
      cr <- (segs$x1[s] - segs$x0[s]) * (cy - segs$y0[s]) -
            (segs$y1[s] - segs$y0[s]) * (cx - segs$x0[s])
      if (nrow(segs) == 1L) {
        # a lone chord separates the two sign groups completely
        expect_true(all(l[cr > 0] == segs$left[s]))
        expect_true(all(l[cr < 0] == segs$right[s]))
      } else {
        # saddle segments: the labels describe the adjacent regions, so each
        # side label must occur among the corners strictly on that side (the
        # opposite cut corner also lies beyond the segment's line)
        expect_true(segs$left[s] %in% l[cr > 0])
        expect_true(segs$right[s] %in% l[cr < 0])
      }
    }
  }
})

test_that("discrete cell segments match iso_discrete() topology in all 15 cases", {
  pats <- list(
    c(1, 1, 1, 1), c(1, 1, 1, 2), c(1, 1, 2, 1), c(1, 2, 1, 1), c(1, 1, 2, 2),
    c(1, 2, 1, 2), c(1, 2, 2, 1), c(1, 2, 2, 2), c(1, 1, 2, 3), c(1, 2, 1, 3),
    c(1, 2, 3, 1), c(1, 2, 2, 3), c(1, 2, 3, 2), c(1, 2, 3, 3), c(1, 2, 3, 4)
  )
  for (p in pats) {
    l <- letters[p]
    segs <- cell_segments(unit_prep(l, class = "discrete"))
    oracle <- iso_discrete(data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0),
                                      id = "t", value = l))
    expect_identical(
      seg_keys(segs$x0, segs$y0, segs$x1, segs$y1),
      oracle_keys(oracle),
      info = paste("pattern", paste(p, collapse = ","))
    )
  }
})

test_that("discrete segment side labels are geometrically correct", {
  cx <- c(0, 0, 1, 1); cy <- c(0, 1, 1, 0)
  for (p in list(c(1, 1, 2, 3), c(1, 2, 1, 3), c(1, 2, 3, 2), c(1, 2, 3, 4),
                 c(1, 2, 1, 2), c(1, 1, 2, 2))) {
    l <- letters[p]
    segs <- cell_segments(unit_prep(l, class = "discrete"))
    for (s in seq_len(nrow(segs))) {
      cr <- (segs$x1[s] - segs$x0[s]) * (cy - segs$y0[s]) -
            (segs$y1[s] - segs$y0[s]) * (cx - segs$x0[s])
      # each side label belongs to a corner strictly on that side
      expect_true(segs$left[s] %in% l[cr > 0])
      expect_true(segs$right[s] %in% l[cr < 0])
    }
  }
})
