#' Find points on boundary of zero set of function
#' 
#'
#' @section Arguments: `biroot_lines()` understands the following arguments:
#'
#'   - f - function with zero set within region
#'   - xlim - boundaries of x to be searched
#'   - ylim - boundaries of y to be searched
#'   - max_depth - how many times should the grid be split in four
#'   - min_depth - how many times should the grid split before becoming adaptive
#'   - ... - additional arguments for f
#'   
#' @returns A dataframe with a row for each point with columns giving, location,
#'  how deep in the grid search they were, a unique id for pair of points.
#'
#' @name biroot_lines
#'
#'
#' @examples
#' 
#' ftest <- function(x) x$x^2-x$y-2
#' biroot(sq, ftest)
#' biroot(sq, ftest) |> str()
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
#' df |> 
#'   ggplot(aes(x, y)) +
#'     # geom_point( data = df |> filter(abs(value) >  .001), color = "black" ) +
#'     geom_point( data = df |> filter(abs(value) <= .001), color = "firebrick1" ) +
#'     coord_equal()
#'     
#'  biroot(f = heartf, xlim = c(-1.5,1.5),
#'         ylim = c(-1,1.5), max_depth = 8, boundary = TRUE) |> 
#'  ggplot(aes(x, y, group = id))+
#'     geom_line()
    


#' @rdname biroot
#' @export
biroot_lines <- function(f, xlim = c(-1,1), ylim = c(-1,1) ,max_depth = 10, min_depth = 2, ...) {
    final <- subset(biroot(f = f, xlim = xlim, ylim = ylim, 
                           max_depth = max_depth, min_depth = min_depth,
                           ...),depth == max_depth)
    na.omit(do.call(rbind,lapply(split.data.frame(final,0:(nrow(final)-1) %/% 4),iso)))
}

iso <- function(df){
  value <- df$value
  x <- df$x
  y <- df$y
  id <- df$id[1]
  signs <- sign(df$value)
  midx <- (x[1]+x[3])/2
  midy <- (y[1]+y[2])/2
  if(all(signs == c(1,1,1,1))) return(data.frame(x = NA,y = NA,id = id))
  if(all(signs == c(-1,-1,-1,-1))) return(data.frame(x = NA,y = NA,id = id))
  if(all(signs == c(-1,1,1,1))) return(
    data.frame(x = c(midx,x[1]),y = c(y[1],midy),id = id))
  if(all(signs == c(1,-1,1,1))) return(
    data.frame(x = c(x[1],midx),y = c(midy,y[2]),id = id))
  if(all(signs == c(1,1,-1,1))) return(
    data.frame(x = c(midx,x[3]),y = c(y[2],midy),id = id))
  if(all(signs == c(1,1,1,-1))) return(
    data.frame(x = c(midx,x[3]),y = c(y[1],midy),id = id))
  if(all(signs == c(-1,-1,1,1))) return(
    data.frame(x = c(midx,midx),y = c(y[1],y[2]),id = id))
  #Special Case
  if(all(signs == c(-1,1,-1,1))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[1],y[2],midy),
    id = c(id,id,paste0(id,"-2"))))
  if(all(signs == c(-1,1,1,-1))) return(
    data.frame(x = c(x[1],x[3]),y = c(midy,midy),id = id))
  if(all(signs == c(1,-1,-1,1))) return(
    data.frame(x = c(x[1],x[3]),y = c(midy,midy),id = id))
  #Special Case
  if(all(signs == c(1,-1,1,-1))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[2],y[1],midy),
    id = c(id,id,paste0(id,"-2"))))
  if(all(signs == c(1,1,-1,-1))) return(
    data.frame(x = c(midx,midx),y = c(y[1],y[2]),id = id))
  if(all(signs == c(-1,-1,-1,1))) return(
    data.frame(x = c(midx,x[3]),y = c(y[1],midy),id = id))
  if(all(signs == c(-1,-1,1,-1))) return(
    data.frame(x = c(midx,x[3]),y = c(y[2],midy),id = id))
  if(all(signs == c(-1,1,-1,-1))) return(
    data.frame(x = c(x[1],midx),y = c(midy,y[2]),id = id))
  if(all(signs == c(1,-1,-1,-1))) return(
    data.frame(x = c(x[1],midx),y = c(midy,y[1]),id = id))
}