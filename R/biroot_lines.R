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
biroot_lines <- function(f, xlim = c(-1,1), ylim = c(-1,1) ,max_depth = 10,
                         min_depth = 2, class = "continuous", ...) {
    final <- subset(biroot(f = f, xlim = xlim, ylim = ylim, 
                           max_depth = max_depth, min_depth = min_depth,
                           class = class, ...),depth == max_depth)
    if (class == "continuous"){
      na.omit(do.call(rbind,lapply(split.data.frame(final,0:(nrow(final)-1) %/% 4),iso))) |> 
        resort()
    }
    else{
      na.omit(do.call(rbind,lapply(split.data.frame(final,0:(nrow(final)-1) %/% 4),iso_discrete))) |> 
        resort()
    }
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
    id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(signs == c(-1,1,1,-1))) return(
    data.frame(x = c(x[1],x[3]),y = c(midy,midy),id = id))
  if(all(signs == c(1,-1,-1,1))) return(
    data.frame(x = c(x[1],x[3]),y = c(midy,midy),id = id))
  #Special Case
  if(all(signs == c(1,-1,1,-1))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[2],y[1],midy),
    id = c(id,id,rep(paste0(id,"-2"),2))))
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

iso_discrete <- function(df){
  values <- as.numeric(factor(df$value, levels = unique(df$value)))
  x <- df$x
  y <- df$y
  id <- df$id[1]
  midx <- (x[1]+x[3])/2
  midy <- (y[1]+y[2])/2
  if(all(values == c(1,1,1,1))) return(data.frame(x = NA,y = NA,id = id))
  if(all(values == c(1,1,1,2))) return(
    data.frame(x = c(midx,x[3]),y = c(y[1],midy),id = id))
  if(all(values == c(1,1,2,1))) return(
    data.frame(x = c(midx,x[3]),y = c(y[2],midy),id = id))
  if(all(values == c(1,2,1,1))) return(
    data.frame(x = c(x[1],midx),y = c(midy,y[2]),id = id))
  if(all(values == c(1,1,2,2))) return(
    data.frame(x = c(midx,midx),y = c(y[1],y[2]),id = id))
  #Special case
  if(all(values == c(1,2,1,2))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[2],y[1],midy),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,2,1))) return(
    data.frame(x = c(x[1],x[3]),y = c(midy,midy),id = id))
  if(all(values == c(1,2,2,2))) return(
    data.frame(x = c(x[1],midx),y = c(midy,y[1]),id = id))
  if(all(values == c(1,1,2,3))) return(
    data.frame(x = c(midx,midx,midx,x[3]),y = c(y[1],y[2],midy,midy),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,1,3))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[2],y[1],midy),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,3,1))) return(
    data.frame(x = c(x[1],x[3],midx,midx),y = c(midy,midy,midy,y[2]),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,2,3))) return(
    data.frame(x = c(x[1],x[3],midx,midx),y = c(midy,midy,midy,y[1]),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,3,2))) return(
    data.frame(x = c(x[1],midx,midx,x[3]),y = c(midy,y[1],y[2],midy),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,3,3))) return(
    data.frame(x = c(x[1],midx,midx,midx),y = c(midy,midy,y[1],y[2]),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  if(all(values == c(1,2,3,4))) return(
    data.frame(x = c(x[1],x[2],midx,midx),y = c(midy,midy,y[1],y[2]),
               id = c(id,id,rep(paste0(id,"-2"),2))))
  else data.frame(x = values, y = df$value, id = "forgot case")
}

resort <- function(x){
  #x <- arrange(x,id,x,y)
  x[,1:2] <- zapsmall(x[,1:2])
  x$xy <- x$x+x$y
  x$order <- NA
  x$order[1:2] <- 1:2
  x$line <- NA
  chunk <- 1
  for (i in 1:(nrow(x)/2-1)) {
    j <- 1
    while (any(x[2*i,1:2] != x[j,1:2])) {
      j <- j+1
      if(2*i == j) j <- j+1
      if(j > nrow(x)) {
        print(paste0("failed at ",2*i))
        chunk <- chunk + 1
        break
      }
    }
    if(j < nrow(x)){
      if(x[j,"id"] == x[j+1,"id"]){
        x[c(j,j+1),]$order <- c(2*i+1,2*i+2)
        x[c(j,j+1),]$line <- chunk 
      }
      if(j>1){
        if(x[j,"id"] == x[j-1,"id"]){
          x[c(j,j-1),]$order <- c(2*i+1,2*i+2)
          x[c(j,j-1),]$line <- chunk 
        } 
        if(x[j,"id"] != x[j-1,"id"] & x[j,"id"] != x[j+1,"id"]) print(paste0("oh no ",j))
        x
      }
      if(abs(x[2*i,"xy"] - x[2*i+2,"xy"]) > 0.1) chunk <- chunk + 1
    }
    x
  }
  x
}