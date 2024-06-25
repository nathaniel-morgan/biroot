#' Return lines approximating the solution set of the function given
#' within the region given.
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
#' ftest <- function(x) with(x, x^2+y^2-1)
#' solution_lines <- biroot_lines(ftest, xlim = c(-2,2), ylim = c(-2,2))
#' solution_lines |> str()
#' 
#' solution_lines |> 
#'   ggplot(aes(x, y, group = id)) +
#'     geom_line() +
#'     coord_equal()
#'   

    


#' @rdname biroot_lines
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