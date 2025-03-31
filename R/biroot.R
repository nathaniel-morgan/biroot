#' Find points on boundary of discrete classification
#' 
#'
#' @section Arguments: `biroot()` understands the following arguments:
#'
#'   - f - function for discrete classification over region
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
  
  sq <- expand.grid(x=xlim,y=ylim)[c(1,3,4,2),]
  sq$id <- "0-1-1"
  sq$depth <- 0
  sq$row <- 1
  sq$col <- 1
  f_new <- function(x) f(x, ...)
  sq$value <- f_new(sq[,c("x","y")])
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
      if ( all(sign(sq$value) == sign(sq$value[1])) & depth > min_depth ) return(sq)
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
  
  for (i in 1:(max_depth-1)) {
    temp <- subset(output, depth == i)
    temp <- split.data.frame(temp, 0:(nrow(temp)-1) %/% 4)
    output <- rbind(
      subset(output, depth != i),
      do.call(
        rbind, 
        lapply(temp, split_one, f = f_new, depth = i)
      )
    )
  }
  output
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