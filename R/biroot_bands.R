#' Region polygons of a biroot search
#'
#' `biroot_bands()` runs (or reuses) a [biroot()] adaptive search and assembles
#' the window into merged region polygons — one face per connected region of
#' constant label, with holes — suitable for `ggplot2::geom_polygon()`. For a
#' continuous function the regions are \eqn{\{f \ge 0\}} and \eqn{\{f < 0\}}
#' within the window, bounded by interpolated contours; for a discrete
#' classification they are the decision regions of each class, bounded at
#' cell-edge midpoints.
#'
#' Plot with
#' `geom_polygon(aes(x, y, group = id, subgroup = subid, fill = factor(value)))`;
#' the `subgroup` aesthetic together with `geom_polygon()`'s default
#' `"evenodd"` fill rule renders the holes. Regions enclosed by another region
#' (islands) are separate faces, so the full output tiles the window exactly.
#'
#' See [biroot_paths()] for the determinism requirement on `object` and for
#' the recommended shared-[biroot()] workflow.
#'
#' @inheritParams biroot_paths
#'
#' @returns A data frame with one row per polygon vertex, in ring order:
#'   - `x`, `y`: vertex location,
#'   - `id`: integer face identifier (map to `group`),
#'   - `subid`: integer ring identifier within the face (map to `subgroup`);
#'     ring 1 is the outer boundary (counterclockwise), rings 2+ are holes
#'     (clockwise); rings do not repeat their first vertex,
#'   - `value`: the region label (`-1`/`1` for continuous input, the class
#'     labels for discrete input).
#'
#' @seealso [biroot_paths()] for the boundary curves themselves.
#'
#' @examples
#' # a generous min_depth ensures nearby contour branches are all detected
#' f <- function(v) (v[1]^2 + v[2]^2 - 1)^3 - v[1]^2 * v[2]^3
#' bands <- biroot_bands(f, xlim = c(-2, 2), ylim = c(-2, 2),
#'                       max_depth = 7, min_depth = 4)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library("ggplot2")
#'   ggplot(bands, aes(x, y, group = id, subgroup = subid)) +
#'     geom_polygon(aes(fill = factor(value)), color = NA) +
#'     coord_equal()
#' }
#'
#' # discrete decision regions of a nearest-centroid classifier
#' centers <- data.frame(x = c(-1, 1, 0), y = c(-.5, -.5, 1), class = c("a", "b", "c"))
#' classify <- function(v) {
#'   d <- outer(v$x, centers$x, "-")^2 + outer(v$y, centers$y, "-")^2
#'   centers$class[max.col(-d, ties.method = "first")]
#' }
#' regions <- biroot_bands(classify, xlim = c(-2, 2), ylim = c(-2, 2),
#'                         max_depth = 6, class = "discrete")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot(regions, aes(x, y, group = id, subgroup = subid)) +
#'     geom_polygon(aes(fill = value), color = NA) +
#'     coord_equal()
#' }
#'
#' @export
biroot_bands <- function(object, xlim = c(-1, 1), ylim = c(-1, 1),
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

  crack <- vapply(st$chains, `[[`, logical(1), "crack")
  sliver <- vapply(st$chains, `[[`, logical(1), "sliver")
  if (any(crack))
    warning("some boundary chains dead-end mid-window and are excluded from ",
            "region assembly; increase min_depth to avoid this.", call. = FALSE)
  face_chains <- st$chains[!crack & !sliver]

  boundary_keys <- unique(unlist(lapply(st$chains, function(ch) pt_key(ch$x, ch$y))))
  arcs <- border_ring(face_chains, prep, exclude = boundary_keys)
  rings <- walk_faces(face_chains, arcs, prep)
  faces <- nest_rings(rings)

  rows <- list()
  for (i in seq_along(faces)) {
    fc <- faces[[i]]
    rings_i <- c(list(fc$outer), fc$holes)
    for (j in seq_along(rings_i)) {
      r <- rings_i[[j]]
      rows[[length(rows) + 1L]] <-
        data.frame(x = r$x, y = r$y, id = i, subid = j, value = fc$outer$label)
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
