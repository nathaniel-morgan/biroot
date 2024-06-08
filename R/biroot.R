#' Find points on boundary of discrete classification
#' 
#'
#' @section Arguments: `biroot()` understands the following arguments:
#'
#'   - sq - corners of the region to be searched
#'   - f - function for discrete classification over region
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
#' sq <- expand.grid(x = c(-5,5), y = c(-5,5))
#' ftest <- function(x) x$x^2-x$y-2
#' biroot(sq, ftest)
#' 
#' biroot(sq, ftest) |> 
#'   ggplot(aes(x, y)) +
#'     geom_point(aes(fill = value), shape = 21) +
#'     coord_equal()
#'   
#'   
#' sq <- expand.grid(x = c(-2,2), y = c(-2,2))
#' ftest <- function(v) with(v, x^2 + y^2 - 1)
#' 
#' (df <- biroot(sq, ftest, max_depth = 9) )
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
#' df |> 
#'   ggplot(aes(x, y)) +
#'     # geom_point( data = df |> filter(abs(value) >  .001), color = "black" ) +
#'     geom_point( data = df |> filter(abs(value) <= .001), color = "firebrick1" ) +
#'     coord_equal()
    


#' @rdname biroot
#' @export
biroot <- function(sq, f, max_depth = 5, min_depth = 2, ...) {
  
  sq$id <- 0
  sq$depth <- 0
  f_new <- function(x) f(x, ...)
  sq$value <- NA
  sq$value <- f_new(sq[,c("x","y")])
  
  process_quadpoint <- function(df, depth) {
    cbind(
      df, # data frame with columns x and y
      "id" = paste0(depth + 1, "-", runif(1)),
      "depth" = depth + 1, 
      "value" = f_new(df[,c("x","y")])
    )
  }
  
  split_one <- function(sq, f, depth) {
    cls <- f(sq[,c("x","y")])
    if ( all(sign(cls) == sign(cls[1])) && depth > min_depth ) return(sq)
    rbind(sq, do.call(rbind, lapply(quad_points(sq), process_quadpoint, depth = depth)))
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