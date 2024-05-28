#' Find points on boundary of discrete classification
#' 
#'
#' @section Arguments: `quadroot_discrete()` understands the following arguments:
#'
#'   - x - The region to be searched
#'
#' @name quadroot_discrete
#'
#'
#' @examples
#' # Basic test
#' 
#' set.seed(17)
#' 1+1
#' 

#' @rdname quadroot_discrete
#' @format NULL
#' @usage NULL
#' @export
quadroot_discrete <- function(sq, f, max_depth = 5, min_depth = 2, ...) {
  sq$id <- 0
  sq$depth <- 0
  sq$class <- f(sq,...)
  process_quadpoint <- function(x, depth) {
    cbind(x, id = paste0(depth + 1, "-", runif(1)),
          depth = depth + 1, class = f(x, ...))
  }
  split_one <- function(sq, f, depth, max_depth, min_depth, ...) {
    cls <- f(sq, ...)
    if (!all(cls == cls[1]) | depth <= min_depth) {
      return(rbind(sq, Reduce(rbind, lapply(quad_points(sq), process_quadpoint, depth = depth))))
    } else {
      return(sq)
    }
  }
  
  output <- split_one(sq = sq, f = f, depth = 0, max_depth = max_depth, min_depth = min_depth, ...)
  
  for (i in 1:(max_depth-1)) {
    temp <- subset(output, output$depth == i)
    temp <- split.data.frame(temp, 0:(nrow(temp)-1) %/% 4)
    output <- rbind(subset(output, output$depth != i), Reduce(rbind, lapply(temp, split_one, f = f, depth = i, max_depth = max_depth, min_depth = min_depth, ...)))
  }
  output
}

quad_points <- function(df){
  list(data.frame(x=df$x+df$x[1],
                  y=df$y+df$y[1])/2,
       data.frame(x=df$x+df$x[3],
                  y=df$y+df$y[3])/2,
       data.frame(x=df$x+df$x[4],
                  y=df$y+df$y[4])/2,
       data.frame(x=df$x+df$x[2],
                  y=df$y+df$y[2])/2)
}