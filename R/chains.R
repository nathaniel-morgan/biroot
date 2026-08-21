# Shared engine for biroot_paths() and biroot_bands(): resolve input, extract
# leaf cells, emit oriented labeled boundary segments, and stitch them into
# chains. All point matching is exact (hex-float keys) — crossing coordinates
# are computed identically from bitwise-shared corner data on both sides of
# every cell edge, so no tolerances appear anywhere in this file.

pt_key <- function(x, y) paste(sprintf("%a", x + 0), sprintf("%a", y + 0))

# corner columns are ordered bl, tl, tr, br; edge e joins corners e and e%%4+1
# (1 = left, 2 = top, 3 = right, 4 = bottom); corner k touches edges k and
# k-1 (cyclically)
EDGE_I <- c(1L, 2L, 3L, 4L)
EDGE_J <- c(2L, 3L, 4L, 1L)


# bisection refinement of discrete boundary crossings: for every straddling
# leaf-cell edge, binary-search the label change with `k` batched calls to
# the classifier, localizing the crossing to edge-length/2^(k+1) instead of
# the midpoint's edge-length/2. Computed once per canonical edge so the two
# cells sharing it reuse the bitwise-identical point. An edge whose bracket
# midpoint classifies to a third label is frozen where it stands.
refine_crossings <- function(X, Y, L, f, k) {
  mixed <- which(L[, 1] != L[, 2] | L[, 1] != L[, 3] | L[, 1] != L[, 4])
  x0 <- y0 <- x1 <- y1 <- numeric(0)
  l0 <- l1 <- character(0)
  for (e in 1:4) {
    i <- EDGE_I[e]; j <- EDGE_J[e]
    r <- mixed[L[mixed, i] != L[mixed, j]]
    x0 <- c(x0, X[r, i]); y0 <- c(y0, Y[r, i]); l0 <- c(l0, L[r, i])
    x1 <- c(x1, X[r, j]); y1 <- c(y1, Y[r, j]); l1 <- c(l1, L[r, j])
  }
  swap <- x1 < x0 | (x1 == x0 & y1 < y0)
  tmp <- x0[swap]; x0[swap] <- x1[swap]; x1[swap] <- tmp
  tmp <- y0[swap]; y0[swap] <- y1[swap]; y1[swap] <- tmp
  tmpl <- l0[swap]; l0[swap] <- l1[swap]; l1[swap] <- tmpl

  key <- paste(pt_key(x0, y0), pt_key(x1, y1))
  first <- !duplicated(key)
  key <- key[first]
  x0 <- x0[first]; y0 <- y0[first]; l0 <- l0[first]
  x1 <- x1[first]; y1 <- y1[first]; l1 <- l1[first]

  active <- rep(TRUE, length(key))
  for (round in seq_len(k)) {
    if (!any(active)) break
    mx <- (x0[active] + x1[active]) / 2
    my <- (y0[active] + y1[active]) / 2
    lm <- as.character(f(data.frame(x = mx, y = my)))
    eq0 <- lm == l0[active]
    eq1 <- lm == l1[active]
    idx <- which(active)
    x0[idx[eq0]] <- mx[eq0]; y0[idx[eq0]] <- my[eq0]
    x1[idx[eq1]] <- mx[eq1]; y1[idx[eq1]] <- my[eq1]
    active[idx[!eq0 & !eq1]] <- FALSE
  }

  cx <- (x0 + x1) / 2; cy <- (y0 + y1) / 2
  env <- new.env(parent = emptyenv(), size = length(key))
  for (i in seq_along(key)) assign(key[i], c(cx[i], cy[i]), envir = env)
  env
}


# resolve a function-or-data-frame `object` into leaf-cell matrices plus
# window and label metadata; `class` is NULL when the caller didn't supply it
biroot_prep <- function(object, xlim, ylim, max_depth, min_depth, class = NULL,
                        refine = 0, refine_explicit = FALSE, ...) {

  if (is.function(object)) {
    if (is.null(class)) class <- "continuous"
    df <- biroot(object, xlim = xlim, ylim = ylim, max_depth = max_depth,
                 min_depth = min_depth, class = class, ...)
  } else if (is.data.frame(object)) {
    df <- object
    req <- c("x", "y", "id", "depth", "value", "parent", "position")
    if (!all(req %in% names(df))) {
      if ("class" %in% names(df) && !("position" %in% names(df)))
        stop("`object` looks like biroot_discrete() output, which lacks the columns ",
             "needed here; recompute with biroot(class = \"discrete\").", call. = FALSE)
      stop("`object` must be a function or a biroot() data frame; missing column(s): ",
           paste(setdiff(req, names(df)), collapse = ", "), ".", call. = FALSE)
    }
    meta <- attr(df, "biroot")
    if (is.null(class)) {
      class <- if (!is.null(meta)) meta$class
        else if (is.character(df$value) || is.factor(df$value)) "discrete"
        else "continuous"
    }
    if (class == "continuous" && (is.character(df$value) || is.factor(df$value)))
      stop("`class = \"continuous\"` contradicts the character `value` column in `object`; ",
           "use class = \"discrete\".", call. = FALSE)
    xlim <- range(df$x[df$depth == 0])
    ylim <- range(df$y[df$depth == 0])
  } else {
    stop("`object` must be a function or a data frame produced by biroot().", call. = FALSE)
  }

  # leaves are squares that were never split, i.e. never appear as a parent
  leaf_ids <- setdiff(unique(df$id), unique(as.character(df$parent)))
  lv <- df[df$id %in% leaf_ids, , drop = FALSE]
  lv <- lv[order(lv$id, match(lv$position, c("bl", "tl", "tr", "br"))), , drop = FALSE]
  if (nrow(lv) %% 4L != 0L)
    stop("malformed biroot() output: leaf rows not in groups of 4.", call. = FALSE)

  X <- matrix(lv$x, ncol = 4, byrow = TRUE)
  Y <- matrix(lv$y, ncol = 4, byrow = TRUE)
  if (class == "continuous") {
    L <- matrix(as.integer(sgn(lv$value)), ncol = 4, byrow = TRUE)
    V <- matrix(as.numeric(lv$value), ncol = 4, byrow = TRUE)
  } else {
    L <- matrix(as.character(lv$value), ncol = 4, byrow = TRUE)
    V <- NULL
  }

  # refinement has no effect on continuous input (crossings are already
  # interpolated); for discrete it re-evaluates the classifier, which rides
  # along in the "biroot" attribute for function and data-frame input alike
  crossings <- NULL
  if (refine > 0 && class == "discrete") {
    fstored <- attr(df, "biroot")$f
    if (is.null(fstored)) {
      if (refine_explicit)
        stop("`refine` needs the classifier, but `object` carries none ",
             "(attributes dropped?); pass the function itself or a data frame ",
             "freshly returned by biroot().", call. = FALSE)
      warning("`object` carries no classifier (attributes dropped?), so boundary ",
              "crossings stay at cell-edge midpoints; recompute with biroot() or ",
              "pass the function to enable refinement.", call. = FALSE)
    } else {
      crossings <- refine_crossings(X, Y, L, fstored, refine)
    }
  }

  list(class = class, xlim = xlim, ylim = ylim, X = X, Y = Y, L = L, V = V,
       crossings = crossings)
}


# interpolated crossing on the edge (x0,y0,v0)-(x1,y1,v1), computed in
# canonical (lexicographic) endpoint order so the two cells sharing the edge
# produce bitwise-identical coordinates
edge_crossing <- function(x0, y0, v0, x1, y1, v1) {
  if (x1 < x0 || (x1 == x0 && y1 < y0)) {
    tmp <- x0; x0 <- x1; x1 <- tmp
    tmp <- y0; y0 <- y1; y1 <- tmp
    tmp <- v0; v0 <- v1; v1 <- tmp
  }
  t <- v0 / (v0 - v1)
  c(x0 + t * (x1 - x0), y0 + t * (y1 - y0))
}


# emit oriented labeled segments for the mixed leaf cells; returns a data
# frame with one row per directed segment: endpoints, left/right labels, and
# whether each side label came from a corner strictly off the segment line
cell_segments <- function(prep) {
  X <- prep$X; Y <- prep$Y; L <- prep$L
  continuous <- prep$class == "continuous"

  mixed <- which(L[, 1] != L[, 2] | L[, 1] != L[, 3] | L[, 1] != L[, 4])
  out <- vector("list", length(mixed))

  # side of corner (cx,cy) relative to the directed segment a -> b
  side_of <- function(ax, ay, bx, by, cx, cy)
    sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))

  # build one segment row given endpoints and the corner index sets belonging
  # to each label (la on the strict side of `ca` corners when possible);
  # NULL when the two crossings coincide (zero-length, e.g. both landing on
  # a shared zero-valued corner)
  seg_row <- function(ax, ay, bx, by, la, ca, lb, cb, x, y) {
    if (ax == bx && ay == by) return(NULL)
    sa <- side_of(ax, ay, bx, by, x[ca], y[ca]); sa <- unique(sa[sa != 0])
    sb <- side_of(ax, ay, bx, by, x[cb], y[cb]); sb <- unique(sb[sb != 0])
    if (length(sa) > 1L || length(sb) > 1L)
      stop("internal error: segment does not separate its corner labels.", call. = FALSE)
    if (length(sa) == 0L && length(sb) == 0L)
      stop("internal error: all cell corners lie on a boundary segment.", call. = FALSE)
    if (length(sa) == 0L) sa <- -sb
    if (length(sb) == 0L) sb <- -sa
    strict_a <- any(side_of(ax, ay, bx, by, x[ca], y[ca]) != 0)
    strict_b <- any(side_of(ax, ay, bx, by, x[cb], y[cb]) != 0)
    if (sa == 1) {
      data.frame(x0 = ax, y0 = ay, x1 = bx, y1 = by,
                 left = la, right = lb, left_strict = strict_a, right_strict = strict_b)
    } else {
      data.frame(x0 = ax, y0 = ay, x1 = bx, y1 = by,
                 left = lb, right = la, left_strict = strict_b, right_strict = strict_a)
    }
  }

  for (m in seq_along(mixed)) {
    k <- mixed[m]
    x <- X[k, ]; y <- Y[k, ]; l <- L[k, ]
    straddle <- l[EDGE_I] != l[EDGE_J]
    es <- which(straddle)

    if (continuous) {
      v <- prep$V[k, ]
      cr <- lapply(es, function(e)
        edge_crossing(x[EDGE_I[e]], y[EDGE_I[e]], v[EDGE_I[e]],
                      x[EDGE_J[e]], y[EDGE_J[e]], v[EDGE_J[e]]))
    } else {
      cr <- lapply(es, function(e) {
        xi <- x[EDGE_I[e]]; yi <- y[EDGE_I[e]]
        xj <- x[EDGE_J[e]]; yj <- y[EDGE_J[e]]
        if (!is.null(prep$crossings)) {
          k2 <- if (xj < xi || (xj == xi && yj < yi))
            paste(pt_key(xj, yj), pt_key(xi, yi))
          else
            paste(pt_key(xi, yi), pt_key(xj, yj))
          hit <- get0(k2, envir = prep$crossings)
          if (!is.null(hit)) return(hit)
        }
        c((xi + xj) / 2, (yi + yj) / 2)
      })
    }
    names(cr) <- as.character(es)
    cross_of <- function(e) cr[[as.character(e)]]

    if (length(es) == 2L) {
      # one segment joining the two crossings; corners split into two label
      # groups (guaranteed exactly two labels here)
      a <- cross_of(es[1]); b <- cross_of(es[2])
      la <- l[EDGE_I[es[1]]]
      ca <- which(l == la); cb <- which(l != la)
      out[[m]] <- seg_row(a[1], a[2], b[1], b[2], la, ca, l[cb[1]], cb, x, y)
    } else if (continuous) {
      # continuous saddle: center treated positive, so the two segments cut
      # off the negative corners (matches iso()'s pairing in both patterns)
      neg <- which(l == -1L)
      out[[m]] <- do.call(rbind, lapply(neg, function(k2) {
        e1 <- k2; e0 <- if (k2 == 1L) 4L else k2 - 1L
        a <- cross_of(e0); b <- cross_of(e1)
        seg_row(a[1], a[2], b[1], b[2], l[k2], k2, 1L, setdiff(1:4, k2), x, y)
      }))
    } else if (length(es) == 4L && (l[1] == l[3] || l[2] == l[4])) {
      # discrete: a diagonal pair shares a label; cut off the other two
      # corners, the center joins the shared-diagonal label (matches
      # iso_discrete(), including the 2-class saddle where bl wins)
      lc <- if (l[1] == l[3]) l[1] else l[2]
      cut <- if (l[1] == l[3]) c(2L, 4L) else c(1L, 3L)
      out[[m]] <- do.call(rbind, lapply(cut, function(k2) {
        e1 <- k2; e0 <- if (k2 == 1L) 4L else k2 - 1L
        a <- cross_of(e0); b <- cross_of(e1)
        seg_row(a[1], a[2], b[1], b[2], l[k2], k2, lc, setdiff(1:4, k2), x, y)
      }))
    } else {
      # discrete junction: fan every crossing to the cell center; each spoke
      # separates the two corners of its crossed edge
      cx <- (x[1] + x[3]) / 2; cy <- (y[1] + y[2]) / 2
      out[[m]] <- do.call(rbind, lapply(es, function(e) {
        a <- cross_of(e)
        seg_row(a[1], a[2], cx, cy, l[EDGE_I[e]], EDGE_I[e], l[EDGE_J[e]], EDGE_J[e], x, y)
      }))
    }
  }

  segs <- do.call(rbind, out)
  if (is.null(segs))
    segs <- data.frame(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0), y1 = numeric(0),
                       left = prep$L[integer(0)], right = prep$L[integer(0)],
                       left_strict = logical(0), right_strict = logical(0))
  segs
}


# stitch segments into chains: exact-key nodes, zero-length drop, coincident
# dedup with strict-label merge, splits at degree > 2 nodes and at
# border-lying transitions; returns chains plus the node degree table
stitch_chains <- function(segs, xlim, ylim) {

  k0 <- pt_key(segs$x0, segs$y0)
  k1 <- pt_key(segs$x1, segs$y1)
  keep <- k0 != k1
  segs <- segs[keep, , drop = FALSE]; k0 <- k0[keep]; k1 <- k1[keep]

  # dedupe coincident undirected segments, merging side labels; each side of
  # the merged segment takes the strict label available for it
  ukey <- paste(pmin(k0, k1), pmax(k0, k1))
  if (anyDuplicated(ukey)) {
    grp <- split(seq_along(ukey), ukey)
    keep_idx <- logical(length(ukey))
    for (g in grp) {
      i <- g[1]
      keep_idx[i] <- TRUE
      for (j in g[-1]) {
        flipped <- k0[j] != k0[i]
        j_left <- if (flipped) segs$right[j] else segs$left[j]
        j_left_strict <- if (flipped) segs$right_strict[j] else segs$left_strict[j]
        j_right <- if (flipped) segs$left[j] else segs$right[j]
        j_right_strict <- if (flipped) segs$left_strict[j] else segs$right_strict[j]
        if (!segs$left_strict[i] && j_left_strict) {
          segs$left[i] <- j_left; segs$left_strict[i] <- TRUE
        }
        if (!segs$right_strict[i] && j_right_strict) {
          segs$right[i] <- j_right; segs$right_strict[i] <- TRUE
        }
      }
    }
    segs <- segs[keep_idx, , drop = FALSE]; k0 <- k0[keep_idx]; k1 <- k1[keep_idx]
  }

  on_border <- (segs$y0 == segs$y1 & (segs$y0 == ylim[1] | segs$y0 == ylim[2])) |
               (segs$x0 == segs$x1 & (segs$x0 == xlim[1] | segs$x0 == xlim[2]))

  nodes <- unique(c(k0, k1))
  i0 <- match(k0, nodes); i1 <- match(k1, nodes)
  deg <- tabulate(c(i0, i1), nbins = length(nodes))

  nx <- numeric(length(nodes)); ny <- numeric(length(nodes))
  nx[i0] <- segs$x0; ny[i0] <- segs$y0
  nx[i1] <- segs$x1; ny[i1] <- segs$y1
  node_on_border <- nx == xlim[1] | nx == xlim[2] | ny == ylim[1] | ny == ylim[2]

  # incidence: for each node, the segments touching it
  inc <- split(rep(seq_along(i0), 2L), c(i0, i1))
  incident <- vector("list", length(nodes))
  incident[as.integer(names(inc))] <- inc

  ns <- nrow(segs)
  visited <- logical(ns)

  # walk one chain starting at `node` along segment `s`; stop at a node of
  # degree != 2, at a border-lying flag change, or on returning to the start
  walk <- function(node, s) {
    pts <- node
    left <- NULL; right <- NULL
    repeat {
      visited[s] <<- TRUE
      forward <- i0[s] == node
      nxt <- if (forward) i1[s] else i0[s]
      l <- if (forward) segs$left[s] else segs$right[s]
      r <- if (forward) segs$right[s] else segs$left[s]
      if (is.null(left)) {
        left <- l; right <- r
      } else if (!identical(l, left) || !identical(r, right)) {
        warning("inconsistent side labels along a boundary chain; ",
                "results may be unreliable (try increasing min_depth).", call. = FALSE)
      }
      pts <- c(pts, nxt)
      node <- nxt
      if (deg[node] != 2L) break
      cand <- incident[[node]]
      cand <- cand[!visited[cand]]
      if (length(cand) == 0L) break
      s2 <- cand[1]
      if (on_border[s2] != on_border[s]) break
      s <- s2
    }
    list(nodes = pts, left = left, right = right)
  }

  chains <- list()
  # open chains and junction arms first: start anywhere degree != 2, plus
  # nodes where a border-lying run meets an interior run
  starts <- which(deg != 2L)
  flagged <- unique(c(i0, i1)[c(on_border, on_border) &
                              c(deg[i0] == 2L, deg[i1] == 2L)])
  border_breaks <- flagged[vapply(flagged, function(v) {
    fl <- on_border[incident[[v]]]
    length(unique(fl)) > 1L
  }, logical(1))]
  starts <- unique(c(starts, border_breaks))
  for (v in starts) {
    for (s in incident[[v]]) {
      if (!visited[s]) chains[[length(chains) + 1L]] <- walk(v, s)
    }
  }
  # remaining segments form closed loops of degree-2 nodes
  for (s in seq_len(ns)) {
    if (!visited[s]) chains[[length(chains) + 1L]] <- walk(i0[s], s)
  }

  chains <- lapply(chains, function(ch) {
    v <- ch$nodes
    ch$x <- nx[v]; ch$y <- ny[v]
    ch$closed <- v[1] == v[length(v)] && length(v) > 2L
    ch$on_border <- {
      s1x <- ch$x[1:2]; s1y <- ch$y[1:2]
      (s1y[1] == s1y[2] && (s1y[1] == ylim[1] || s1y[1] == ylim[2])) ||
      (s1x[1] == s1x[2] && (s1x[1] == xlim[1] || s1x[1] == xlim[2]))
    }
    ends <- c(v[1], v[length(v)])
    ch$crack <- !ch$closed && any(deg[ends] == 1L & !node_on_border[ends])
    ch$sliver <- identical(ch$left, ch$right)
    ch
  })

  list(chains = chains, node_x = nx, node_y = ny, deg = deg,
       node_on_border = node_on_border)
}
