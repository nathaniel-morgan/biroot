#' Find points on boundary of discrete classification
#' 
#'
#' @section Arguments: `biroot()` understands the following arguments:
#'
#'   - f - the function to search. Either a function of a single point,
#'     a numeric vector `c(x, y)`, returning one value (numeric for
#'     `class = "continuous"`, a label for `class = "discrete"`); or a
#'     vectorized function mapping a data frame with columns `x` and `y` to
#'     one value per row (detected automatically, evaluated in batches, and
#'     therefore faster — the natural form for `predict()`-based
#'     classifiers).
#'   - xlim - boundaries of x to be searched
#'   - ylim - boundaries of y to be searched
#'   - max_depth - how many times should the grid be split in four
#'   - min_depth - how many times should the grid split before becoming adaptive
#'   - ... - additional arguments for f
#'   
#' @returns A dataframe with a row for each point with columns giving, location,
#'  how deep in the grid search they were, a unique id for each square, 
#'  and finally the result of the discrete function at that point.
#'
#' @name biroot
#'
#'
#' @examples
#' 
#' ftest <- function(x) with(x, x^2-y-2)
#' biroot(ftest, xlim = c(-5,5), ylim = c(-5,5))
#' biroot(ftest, xlim = c(-5,5), ylim = c(-5,5)) |> str()
#' 
#' biroot(ftest, xlim = c(-5,5), ylim = c(-5,5)) |> 
#'   ggplot(aes(x, y)) +
#'     geom_point(aes(fill = value), shape = 21) +
#'     coord_equal()
#'   
#' ftest <- function(v) with(v, x^2 + y^2 - 1)
#' 
#' (df <- biroot(ftest, xlim = c(-2,2), ylim = c(-2,2), max_depth = 9) )
#' 
#' df |> 
#'   ggplot(aes(x, y)) +
#'     geom_point(aes(fill = value), shape = 21) +
#'     geom_contour(
#'       aes(z = z),
#'       color = "red",
#'       breaks = 0, 
#'       data = expand.grid(
#'         "x" = seq(-5, 5, length.out = 101), 
#'         "y" = seq(-5, 5, length.out = 101)
#'       ) |> 
#'         transform("z" = x^2 + y^2 - 1)
#'     ) +  
#'     coord_equal()
#' 
    


#' @rdname biroot
#' @export
biroot <- function(f, xlim = c(-1,1), ylim = c(-1,1) ,max_depth = 10,
                   min_depth = 2, class = "continuous", ...) {

  if (!is.numeric(max_depth) || length(max_depth) != 1L || is.na(max_depth) || max_depth < 1)
    stop("`max_depth` must be a single number >= 1.", call. = FALSE)

  sq <- expand.grid(x=xlim,y=ylim)[c(1,3,4,2),]
  sq$id <- "0-1-1"
  sq$depth <- 0
  sq$row <- 1
  sq$col <- 1
  f_new <- function(x) f(x, ...)
  ad <- adapt_f(f_new, sq[, c("x", "y")])
  f_new <- ad$f
  sq$value <- if (is.null(ad$value)) f_new(sq[, c("x", "y")]) else ad$value
  sq$parent <- 0
  sq$parent_position <- "all"
  sq$position <- c("bl", "tl", "tr", "br")
  
  process_quadpoint <- function(df, depth, sq) {
    rows <- c(sq$row[1]*2-1, sq$row[1]*2-1, sq$row[1]*2, sq$row[1]*2)
    cols <- c(sq$col[1]*2-1, sq$col[1]*2, sq$col[1]*2, sq$col[1]*2-1)
    ids  <- paste0(depth[1] + 1, "-", rows, "-", cols)
    positions <- c("bl", "tl", "tr", "br")
    
    data.frame(
      x = df$x,
      y = df$y,
      id = rep(ids, each = 4),
      depth = depth + 1,
      row = rep(rows, each = 4),
      col = rep(cols, each = 4),
      value = biroot_assign(df = df, sq = sq, f = f_new),
      parent = sq$id,
      parent_position = df$parent_position,
      position = rep(positions, 4)
    )
  }
  
  if (class == "continuous"){
    split_one <- function(sq, f, depth) {
      if ( all(sgn(sq$value) == sgn(sq$value[1])) & depth > min_depth ) return(sq)
      rbind(sq, process_quadpoint(do.call(rbind, quad_point_c(sq)),depth = depth, sq = sq))
    }
  }
  
  else  {
    split_one <- function(sq, f, depth) {
      if ( all(sq$value == sq$value[1]) & depth > min_depth ) return(sq)
      rbind(sq, process_quadpoint(do.call(rbind, quad_point_c(sq)),depth = depth, sq = sq))
    }
  }
  
  output <- split_one(sq = sq, f = f_new, depth = 0)

  for (i in seq_len(max_depth - 1)) {
    temp <- subset(output, depth == i)
    if (nrow(temp) == 0L) break
    temp <- split.data.frame(temp, (seq_len(nrow(temp)) - 1L) %/% 4)
    output <- rbind(
      subset(output, depth != i),
      do.call(
        rbind,
        lapply(temp, split_one, f = f_new, depth = i)
      )
    )
  }
  # f_new (post-adaptation, always data-frame-vectorized) rides along so
  # downstream tools like biroot_paths()/biroot_bands() can re-evaluate the
  # function, e.g. for crossing refinement, without it being passed again
  attr(output, "biroot") <- list(
    class = class, xlim = xlim, ylim = ylim,
    max_depth = max_depth, min_depth = min_depth, f = f_new
  )
  output
}



# zero counts as positive so that refinement and iso() classify corners
# identically and every corner pattern matches one of iso()'s 16 cases
sgn <- function(v) ifelse(v >= 0, 1, -1)



# `f` may be point-wise (numeric c(x, y) -> one value) or vectorized (data
# frame with columns x and y -> one value per row). The vectorized form is
# probed first on the initial corners (it is the faster path, and the form
# predict()-style classifiers take); otherwise the point-wise form is wrapped.
# The choice is made once per search. Returns the adapted function plus the
# probe's corner values when they are reusable.
adapt_f <- function(f, corners) {
  val <- tryCatch(f(corners), error = function(e) e)
  if (is.atomic(val) && length(val) == nrow(corners))
    return(list(f = f, value = val))

  pt <- tryCatch(f(c(corners$x[1], corners$y[1])), error = function(e) e)
  if (is.atomic(pt) && length(pt) == 1L) {
    f_pt <- function(df) {
      out <- vector("list", nrow(df))
      for (i in seq_len(nrow(df))) out[[i]] <- f(c(df$x[i], df$y[i]))
      unlist(out, use.names = FALSE)
    }
    return(list(f = f_pt, value = NULL))
  }

  msg <- paste0("`f` must map a numeric vector c(x, y) to a single value, or ",
                "map a data frame with columns `x` and `y` to one value per row.")
  if (inherits(val, "error"))
    msg <- paste0(msg, " Evaluating f on a data frame failed with: ",
                  conditionMessage(val), ".")
  if (inherits(pt, "error"))
    msg <- paste0(msg, " Evaluating f(c(x, y)) failed with: ",
                  conditionMessage(pt), ".")
  stop(msg, call. = FALSE)
}



quad_points <- function(df){
  with(df,
    list(
      data.frame( "x" = x + x[1], "y" = y + y[1] ) / 2,
      data.frame( "x" = x + x[3], "y" = y + y[3] ) / 2,
      data.frame( "x" = x + x[4], "y" = y + y[4] ) / 2,
      data.frame( "x" = x + x[2], "y" = y + y[2] ) / 2
    )
  )
}

biroot_assign <- function(df,sq,f){
  ml <- f(df[14,c("x","y")])
  mm <- f(df[15,c("x","y")])
  bm <- f(df[16,c("x","y")])
  tm <- f(df[3,c("x","y")])
  mr <- f(df[8,c("x","y")])
  c(ml, sq$value[2], tm, mm,
    mm, tm, sq$value[3], mr,
    bm, mm, mr, sq$value[4],
    sq$value[1], ml, mm, bm)
}