# Face assembly for biroot_bands(): close boundary chains against the window
# border, walk the resulting planar half-edge structure into rings, and nest
# clockwise rings as holes of their containing faces. Border arcs contribute
# only their interior-side half-edge, so the exterior face never materializes.

# perimeter coordinate of border point(s), counterclockwise from the
# bottom-left window corner
perimeter_s <- function(x, y, xlim, ylim) {
  W <- xlim[2] - xlim[1]; H <- ylim[2] - ylim[1]
  ifelse(y == ylim[1] & x < xlim[2], x - xlim[1],
  ifelse(x == xlim[2] & y < ylim[2], W + (y - ylim[1]),
  ifelse(y == ylim[2] & x > xlim[1], W + H + (xlim[2] - x),
         2 * W + H + (ylim[2] - y))))
}

# point on the perimeter at coordinate s
perimeter_pt <- function(s, xlim, ylim) {
  W <- xlim[2] - xlim[1]; H <- ylim[2] - ylim[1]
  s <- s %% (2 * (W + H))
  if (s < W) c(xlim[1] + s, ylim[1])
  else if (s < W + H) c(xlim[2], ylim[1] + (s - W))
  else if (s < 2 * W + H) c(xlim[2] - (s - W - H), ylim[2])
  else c(xlim[1], ylim[2] - (s - 2 * W - H))
}

shoelace <- function(x, y) {
  n <- length(x)
  j <- c(2:n, 1L)
  sum(x * y[j] - x[j] * y) / 2
}


# split the window border into label-uniform arcs between the boundary-chain
# vertices that lie on it; returns a list of arcs (polyline + label), skipping
# spans covered by chain edges that lie on the border itself. `exclude` holds
# point keys of all boundary-chain vertices (including sliver/crack chains):
# leaf corners at those points sit exactly on the boundary, so their labels
# describe neither flanking region and are kept out of the label votes.
border_ring <- function(face_chains, prep, exclude = character(0)) {
  xlim <- prep$xlim; ylim <- prep$ylim
  P <- 2 * (xlim[2] - xlim[1]) + 2 * (ylim[2] - ylim[1])

  on_b <- function(x, y) x == xlim[1] | x == xlim[2] | y == ylim[1] | y == ylim[2]

  # breakpoints: every chain vertex lying exactly on the border (endpoints,
  # tangency vertices, junctions on the border); neighbors along the chain
  # are kept, with their direction, to resolve arc labels at transversal
  # crossings
  bp <- list()
  for (ch in face_chains) {
    hit <- which(on_b(ch$x, ch$y))
    for (i in hit) {
      cand <- c(i + 1L, i - 1L)
      keep2 <- cand >= 1L & cand <= length(ch$x)
      bp[[length(bp) + 1L]] <- list(
        x = ch$x[i], y = ch$y[i],
        nbrs = list(x = ch$x[cand[keep2]], y = ch$y[cand[keep2]],
                    is_after = c(TRUE, FALSE)[keep2],
                    left = ch$left, right = ch$right)
      )
    }
  }

  # leaf corner points on the border, for arc labeling
  onc <- on_b(prep$X, prep$Y)
  cx <- prep$X[onc]; cy <- prep$Y[onc]; cl <- prep$L[onc]
  drop_c <- pt_key(cx, cy) %in% exclude
  if (!all(drop_c)) {
    cx <- cx[!drop_c]; cy <- cy[!drop_c]; cl <- cl[!drop_c]
  }
  cs <- perimeter_s(cx, cy, xlim, ylim)

  full_ring <- function() {
    lab <- unique(cl)
    if (length(lab) > 1L) {
      warning("window border touches more than one region label; ",
              "taking the majority (try increasing min_depth).", call. = FALSE)
      lab <- names(which.max(table(cl)))
      if (is.integer(cl)) lab <- as.integer(lab)
    }
    list(list(x = c(xlim[1], xlim[2], xlim[2], xlim[1]),
              y = c(ylim[1], ylim[1], ylim[2], ylim[2]),
              label = lab, closed = TRUE))
  }

  if (length(bp) == 0L) return(full_ring())

  bx <- vapply(bp, `[[`, numeric(1), "x")
  by <- vapply(bp, `[[`, numeric(1), "y")
  bkey <- pt_key(bx, by)
  first <- !duplicated(bkey)
  bp <- bp[first]; bx <- bx[first]; by <- by[first]
  bs <- perimeter_s(bx, by, xlim, ylim)
  o <- order(bs)
  bp <- bp[o]; bx <- bx[o]; by <- by[o]; bs <- bs[o]
  m <- length(bp)

  # spans covered by border-lying chain edges, matched edge-by-edge (all
  # border-lying chain vertices are breakpoints, so candidate arcs inside a
  # covered span correspond exactly to individual chain edges)
  covered <- character(0)
  for (ch in face_chains) {
    if (!isTRUE(ch$on_border)) next
    for (i in seq_len(length(ch$x) - 1L)) {
      covered <- c(covered, paste(sort(c(pt_key(ch$x[i], ch$y[i]),
                                         pt_key(ch$x[i + 1L], ch$y[i + 1L]))),
                                  collapse = " "))
    }
  }

  corner_s <- perimeter_s(c(xlim[1], xlim[2], xlim[2], xlim[1]),
                          c(ylim[1], ylim[1], ylim[2], ylim[2]), xlim, ylim)

  arcs <- list()
  for (i in seq_len(m)) {
    j <- if (i == m) 1L else i + 1L
    sa <- bs[i]; sb <- if (i == m) bs[j] + P else bs[j]
    if (sb == sa) next  # coincident breakpoints, zero-length arc
    akey <- paste(sort(c(pt_key(bx[i], by[i]), pt_key(bx[j], by[j]))), collapse = " ")
    if (akey %in% covered) next

    # window corners strictly inside the span
    ks <- corner_s + rep(c(0, P), each = 4L)
    inside <- ks > sa & ks < sb
    mids <- sort(ks[inside])
    px <- c(bx[i], vapply(mids, function(s) perimeter_pt(s, xlim, ylim)[1], numeric(1)), bx[j])
    py <- c(by[i], vapply(mids, function(s) perimeter_pt(s, xlim, ylim)[2], numeric(1)), by[j])

    # label: leaf corners strictly inside the span
    css <- c(cs, cs + P)
    cll <- c(cl, cl)
    lab <- unique(cll[css > sa & css < sb])
    if (length(lab) != 1L) {
      # fall back to the side of the chain at the arc's start breakpoint:
      # which side of the incident chain edge does the border's +s tangent
      # direction lie on? (tangent computed exactly from the border side;
      # cr > 0 means the tangent is left of the neighbor direction, which is
      # the chain's left only when the neighbor comes after the breakpoint)
      nb <- bp[[i]]$nbrs
      off <- which(!on_b(nb$x, nb$y))
      ord2 <- c(off, setdiff(seq_along(nb$x), off))
      tng <- if (by[i] == ylim[1] && bx[i] < xlim[2]) c(1, 0)
        else if (bx[i] == xlim[2] && by[i] < ylim[2]) c(0, 1)
        else if (by[i] == ylim[2] && bx[i] > xlim[1]) c(-1, 0)
        else c(0, -1)
      lab2 <- NULL
      for (pick in ord2) {
        cr <- (nb$x[pick] - bx[i]) * tng[2] - (nb$y[pick] - by[i]) * tng[1]
        if (cr != 0) {
          lab2 <- if ((cr > 0) == nb$is_after[pick]) nb$left else nb$right
          break
        }
      }
      if (is.null(lab2)) lab2 <- nb$left
      if (length(lab) > 1L)
        warning("ambiguous region label along the window border; ",
                "results may be unreliable (try increasing min_depth).", call. = FALSE)
      lab <- lab2
    }
    arcs[[length(arcs) + 1L]] <- list(x = px, y = py, label = lab, closed = FALSE)
  }

  if (length(arcs) == 0L) return(full_ring())
  arcs
}


# walk the half-edge structure into rings; chains contribute both sides
# (interior-only when they lie on the border), arcs contribute their interior
# side only
walk_faces <- function(face_chains, arcs, prep) {
  xlim <- prep$xlim; ylim <- prep$ylim

  hx0 <- numeric(0); hy0 <- numeric(0); hx1 <- numeric(0); hy1 <- numeric(0)
  hlab <- list(); htwin <- integer(0)

  add_edge <- function(x0, y0, x1, y1, lab_fwd, lab_rev, two_sided) {
    n <- length(hx0)
    hx0[n + 1L] <<- x0; hy0[n + 1L] <<- y0; hx1[n + 1L] <<- x1; hy1[n + 1L] <<- y1
    hlab[[n + 1L]] <<- lab_fwd
    if (two_sided) {
      hx0[n + 2L] <<- x1; hy0[n + 2L] <<- y1; hx1[n + 2L] <<- x0; hy1[n + 2L] <<- y0
      hlab[[n + 2L]] <<- lab_rev
      htwin[n + 1L] <<- n + 2L; htwin[n + 2L] <<- n + 1L
    } else {
      htwin[n + 1L] <<- NA_integer_
    }
  }

  for (ch in face_chains) {
    np <- length(ch$x)
    for (i in seq_len(np - 1L)) {
      x0 <- ch$x[i]; y0 <- ch$y[i]; x1 <- ch$x[i + 1L]; y1 <- ch$y[i + 1L]
      if (isTRUE(ch$on_border)) {
        # interior-side half-edge only, directed so the window interior is on
        # the left (bottom: +x, top: -x, right: +y, left: -y)
        fwd <- if (y0 == y1) {
          if (y0 == ylim[1]) x1 > x0 else x1 < x0
        } else {
          if (x0 == xlim[2]) y1 > y0 else y1 < y0
        }
        if (fwd) add_edge(x0, y0, x1, y1, ch$left, NULL, FALSE)
        else     add_edge(x1, y1, x0, y0, ch$right, NULL, FALSE)
      } else {
        add_edge(x0, y0, x1, y1, ch$left, ch$right, TRUE)
      }
    }
  }
  for (a in arcs) {
    np <- length(a$x)
    idx <- if (a$closed) c(seq_len(np), 1L) else seq_len(np)
    for (i in seq_len(length(idx) - 1L)) {
      add_edge(a$x[idx[i]], a$y[idx[i]], a$x[idx[i + 1L]], a$y[idx[i + 1L]],
               a$label, NULL, FALSE)
    }
  }

  nh <- length(hx0)
  if (nh == 0L) return(list())

  from_key <- pt_key(hx0, hy0)
  ang <- atan2(hy1 - hy0, hx1 - hx0)
  out_by_vertex <- split(seq_len(nh), from_key)

  # next half-edge continuing the face on the left: first outgoing strictly
  # clockwise from the reversed incoming direction
  next_of <- function(h) {
    vkey <- pt_key(hx1[h], hy1[h])
    cand <- out_by_vertex[[vkey]]
    if (is.null(cand)) stop("internal error: dangling half-edge in face walk.", call. = FALSE)
    ar <- atan2(hy0[h] - hy1[h], hx0[h] - hx1[h])
    a <- ang[cand]
    lower <- cand[a < ar]
    if (length(lower)) lower[which.max(a[a < ar])]
    else cand[which.max(a)]
  }

  visited <- logical(nh)
  rings <- list()
  for (h0 in seq_len(nh)) {
    if (visited[h0]) next
    h <- h0
    xs <- numeric(0); ys <- numeric(0)
    labs <- list(); lens <- numeric(0)
    repeat {
      if (visited[h]) stop("internal error: half-edge revisited in face walk.", call. = FALSE)
      visited[h] <- TRUE
      xs <- c(xs, hx0[h]); ys <- c(ys, hy0[h])
      labs[[length(labs) + 1L]] <- hlab[[h]]
      lens <- c(lens, abs(hx1[h] - hx0[h]) + abs(hy1[h] - hy0[h]))
      h <- next_of(h)
      if (h == h0) break
    }
    ulab <- unique(labs)
    lab <- if (length(ulab) == 1L) ulab[[1]] else {
      warning("conflicting labels within one region; taking the majority ",
              "(try increasing min_depth).", call. = FALSE)
      wt <- tapply(lens, vapply(labs, format, character(1)), sum)
      fmt <- names(which.max(wt))
      labs[[match(fmt, vapply(labs, format, character(1)))]]
    }
    rings[[length(rings) + 1L]] <-
      list(x = xs, y = ys, label = lab, area = shoelace(xs, ys))
  }
  rings
}


# even-odd point-in-polygon; NA when the point lies on the boundary
point_in_ring <- function(px, py, xs, ys) {
  n <- length(xs)
  inside <- FALSE
  j <- n
  for (i in seq_len(n)) {
    cr <- (xs[j] - xs[i]) * (py - ys[i]) - (ys[j] - ys[i]) * (px - xs[i])
    if (cr == 0 &&
        px >= min(xs[i], xs[j]) && px <= max(xs[i], xs[j]) &&
        py >= min(ys[i], ys[j]) && py <= max(ys[i], ys[j])) return(NA)
    if ((ys[i] > py) != (ys[j] > py)) {
      xint <- xs[i] + (py - ys[i]) / (ys[j] - ys[i]) * (xs[j] - xs[i])
      if (px < xint) inside <- !inside
    }
    j <- i
  }
  inside
}


# assign clockwise rings as holes of their smallest containing
# counterclockwise ring with the same label
nest_rings <- function(rings) {
  if (length(rings) == 0L) return(list())
  areas <- vapply(rings, `[[`, numeric(1), "area")
  outer_idx <- which(areas > 0)
  hole_idx <- which(areas <= 0)

  faces <- lapply(outer_idx, function(i) list(outer = rings[[i]], holes = list()))

  for (hi in hole_idx) {
    hole <- rings[[hi]]
    cands <- outer_idx[vapply(outer_idx, function(oi) {
      identical(rings[[oi]]$label, hole$label) && areas[oi] > abs(hole$area)
    }, logical(1))]
    best <- NA_integer_; best_area <- Inf
    for (oi in cands) {
      contained <- NA
      for (k in seq_along(hole$x)) {
        contained <- point_in_ring(hole$x[k], hole$y[k], rings[[oi]]$x, rings[[oi]]$y)
        if (!is.na(contained)) break
      }
      if (is.na(contained)) contained <- TRUE  # every vertex on the boundary
      if (contained && areas[oi] < best_area) {
        best <- oi; best_area <- areas[oi]
      }
    }
    if (is.na(best)) {
      warning("a hole ring could not be matched to a containing region; ",
              "dropping it (try increasing min_depth).", call. = FALSE)
      next
    }
    fi <- match(best, outer_idx)
    faces[[fi]]$holes[[length(faces[[fi]]$holes) + 1L]] <- hole
  }

  faces
}
