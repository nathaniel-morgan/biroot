#' Connected boundary paths of a biroot search
#'
#' `biroot_paths()` runs (or reuses) a [biroot()] adaptive search and stitches
#' the resulting boundary into connected, ordered polylines suitable for
#' `ggplot2::geom_path()`. For continuous functions the boundary is the zero
#' set, with crossing points placed by linear interpolation along cell edges;
#' for discrete classifications it is the decision boundary, with crossings at
#' cell-edge midpoints.
#'
#' Stitching matches coordinates exactly, which requires `object` (when a
#' function) to be deterministic: it is re-evaluated at shared points and must
#' return bitwise-identical values. When calling both `biroot_paths()` and
#' [biroot_bands()] on the same problem, compute `b <- biroot(...)` once and
#' pass the data frame to both.
#'
#' @param object A function of a single point `c(x, y)` returning one value,
#'   a vectorized function mapping a data frame with columns `x` and `y` to
#'   one value per row (as in [biroot()]), or a data frame previously
#'   returned by [biroot()].
#' @param xlim,ylim Numeric vectors of length 2: the window searched. Ignored
#'   (and an error if supplied) when `object` is a data frame.
#' @param max_depth,min_depth Search depths, as in [biroot()]. An error if
#'   supplied when `object` is a data frame.
#' @param class `"continuous"` (zero set of a numeric function) or
#'   `"discrete"` (decision boundary of a labeling function). For data-frame
#'   input the class is taken from the object itself when not supplied; note
#'   a discrete classification encoded with numeric codes cannot be detected
#'   automatically and must be declared with `class = "discrete"`.
#' @param ... Additional arguments passed on to `object` when it is a
#'   function.
#'
#' @returns A data frame with one row per vertex, in path order:
#'   - `x`, `y`: vertex location,
#'   - `id`: integer chain identifier (map to the `group` aesthetic),
#'   - `closed`: whether the chain is a closed loop (closed chains repeat
#'     their first vertex as the last row),
#'   - `left`, `right`: the region labels flanking the chain, relative to its
#'     point order (`-1`/`1` for continuous input, the class labels for
#'     discrete input); `right` is `NA` for chains lying on the window border,
#'   - `crack`: `TRUE` for chains that dead-end mid-window because an
#'     adjacent coarse cell was never refined; increase `min_depth` to avoid
#'     these.
#'
#'   A zero-row data frame is returned when no boundary is found.
#'
#' @seealso [biroot_bands()] for the region polygons bounded by these paths.
#'
#' @examples
#' f <- function(v) v[1]^2 + v[2]^2 - 1
#' paths <- biroot_paths(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6)
#' str(paths)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library("ggplot2")
#'   ggplot(paths, aes(x, y, group = id)) +
#'     geom_path() +
#'     coord_equal()
#' }
#'
#' # reuse one biroot() run for both paths and bands
#' b <- biroot(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 6)
#' identical(biroot_paths(b), paths)
#'
#' @export
biroot_paths <- function(object, xlim = c(-1, 1), ylim = c(-1, 1),
                         max_depth = 10, min_depth = 2,
                         class = "continuous", ...) {

  if (is.data.frame(object) &&
      (!missing(xlim) || !missing(ylim) || !missing(max_depth) || !missing(min_depth)))
    stop("when `object` is a biroot() data frame, `xlim`, `ylim`, `max_depth`, ",
         "and `min_depth` come from it and must not be supplied.", call. = FALSE)

  prep <- biroot_prep(object, xlim, ylim, max_depth, min_depth,
                      class = if (missing(class)) NULL else class, ...)
  segs <- cell_segments(prep)
  st <- stitch_chains(segs, prep$xlim, prep$ylim)
  chains <- st$chains

  na_label <- if (prep$class == "continuous") NA_integer_ else NA_character_

  if (length(chains) == 0L)
    return(data.frame(x = numeric(0), y = numeric(0), id = integer(0),
                      closed = logical(0), left = na_label[0], right = na_label[0],
                      crack = logical(0)))

  if (any(vapply(chains, `[[`, logical(1), "crack")))
    warning("some boundary chains dead-end mid-window (a neighboring cell was ",
            "never refined); increase min_depth to avoid this.", call. = FALSE)

  rows <- lapply(seq_along(chains), function(i) {
    ch <- chains[[i]]
    x <- ch$x; y <- ch$y; left <- ch$left; right <- ch$right
    if (isTRUE(ch$on_border)) {
      # orient border-lying chains with the window interior on the left,
      # then the (exterior) right side carries no region
      interior_left <- {
        x0 <- x[1]; y0 <- y[1]; x1 <- x[2]; y1 <- y[2]
        if (y0 == y1) { if (y0 == prep$ylim[1]) x1 > x0 else x1 < x0 }
        else { if (x0 == prep$xlim[2]) y1 > y0 else y1 < y0 }
      }
      if (!interior_left) {
        x <- rev(x); y <- rev(y)
        tmp <- left; left <- right; right <- tmp
      }
      right <- na_label
    }
    data.frame(x = x, y = y, id = i, closed = ch$closed,
               left = left, right = right, crack = ch$crack)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
