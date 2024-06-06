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
#' # using it to plot boundaries of qda
#' 
#' set.seed(17)
#' 1+1


#' @rdname biroot
#' @format NULL
#' @usage NULL
#' @export
biroot <- function(sq, f, max_depth = 5, min_depth = 2, ...) {
  sq$id <- 0
  sq$depth <- 0
  f_new <- function(x) f(x, ...)
  sq$value <- NA
  sq$value <- f_new(sq[,c("x","y")])
  process_quadpoint <- function(x, depth) {
    cbind(x, id = paste0(depth + 1, "-", runif(1)),
          depth = depth + 1, value = f_new(x[,c("x","y")]))
  }
  
  split_one <- function(sq, f, depth, max_depth, min_depth) {
    cls <- f(sq[,c("x","y")])
    if (!all(sign(cls) == sign(cls[1])) | depth <= min_depth) {
      return(rbind(sq, do.call(rbind, lapply(quad_points(sq), process_quadpoint, depth = depth))))
    } else {
      return(sq)
    }
  }
  
  output <- split_one(sq = sq, f = f_new, depth = 0, max_depth = max_depth, min_depth = min_depth)
  
  for (i in 1:(max_depth-1)) {
    temp <- subset(output, output$depth == i)
    temp <- split.data.frame(temp, 0:(nrow(temp)-1) %/% 4)
    output <- rbind(subset(output, output$depth != i),
                    do.call(rbind, lapply(temp, split_one, f = f_new,
                                          depth = i, max_depth = max_depth,
                                          min_depth = min_depth)))
  }
  output
}
