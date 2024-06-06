#' Find points on boundary of discrete classification
#' 
#'
#' @section Arguments: `biroot_discrete()` understands the following arguments:
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
#' @name biroot_discrete
#'
#'
#' @examples
#' # using it to plot boundaries of qda
#' 
#' set.seed(17)
#' df <- data.frame(x = rnorm(40), y=rnorm(40) + 2, class = factor("A")) |> 
#'   rbind(data.frame(x = rnorm(40, sd=2) + 2, y=rnorm(40, sd = 2) - 1, class = factor("B"))) |> 
#'   rbind(data.frame(x = rnorm(40, sd=1/2) + 2, y=rnorm(40, sd = 1/2) + 1,class = factor("C")))
#' 
#' names(df)[3] <- "fill"
#' 
#' disc_qda <- do.call(MASS::qda, args = list(fill~., data=df))
#' 
#' qda_square <- expand.grid(x=c(min(df$x),max(df$x)),
#'                           y=c(min(df$y),max(df$y)))[c(1,3,4,2),]
#' 
#' qda_pred <- function(x, mod){
#'   as.character(predict(mod,x)$class)
#' }
#' 
#' df_test <- biroot_discrete(sq = qda_square, f = qda_pred, mod = disc_qda, max_depth = 8)
#' df_test |> ggplot(aes(x,y, fill = class , group = id, col = class))+
#'   geom_polygon()
#' 

#' @rdname biroot_discrete
#' @format NULL
#' @usage NULL
#' @export
biroot_discrete <- function(sq, f, max_depth = 5, min_depth = 2, ...) {
  sq$id <- 0
  sq$depth <- 0
  f_new <- function(x) f(x, ...)
  sq$class <- NA
  sq$class <- f_new(sq[,c("x","y")])
  
  process_quadpoint <- function(x, depth) {
    cbind(x, id = paste0(depth + 1, "-", runif(1)),
          depth = depth + 1, class = f_new(x[,c("x","y")]))
  }
  
  split_one <- function(sq, f, depth, max_depth, min_depth) {
    cls <- f(sq[,c("x","y")])
    if (!all(cls == cls[1]) | depth <= min_depth) {
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

quad_points <- function(df){
  with(df,
       list(data.frame( "x" = x + x[1],
                        "y" = y + y[1])/2,
            data.frame( "x" = x + x[3],
                        "y" = y + y[3])/2,
            data.frame( "x" = x + x[4],
                        "y" = y + y[4])/2,
            data.frame( "x" = x + x[2],
                        "y" = y + y[2])/2))
}

match_row_in_matrix <- function(x, A, tol = sqrt(.Machine$double.eps)) {
  if (nrow(A) == 0L) return( NA_integer_ )
  for (i in 1:nrow(A)) {
    if (all(abs(x - A[i,]) <= tol)) return(i)
  }
  NA_integer_
}

memorize <- function(f, n, m, simplify = TRUE) {
  
  x_log <- matrix(nrow = 0, ncol = n)
  y_log <- matrix(nrow = 0, ncol = m)
  
  fm <- function(x) {
    
    input_was_vector <- is.vector(x)
    if (input_was_vector) {
      if (n == 1L) x <- t(t(x)) else x <- t(x)
    }
    
    N <- nrow(x)
    y <- matrix(nrow = N, ncol = m)
    
    # iterate
    for (i in 1:N) {
      mtch <- match_row_in_matrix(x[i,], x_log)
      if (!is.na(mtch)) {                     # if value already computed, recall it
        y[i,] <- y_log[mtch,]
      } else {                                # if not on file, compute it and store
        y[i,] <- f(x[i,])
        x_log <<- rbind(x_log, x[i,])
        y_log <<- rbind(y_log, y[i,])
      }
    }
    
    if (simplify && input_was_vector) {
      if (n == 1L) return (y[,1])
      if (N == 1L) return (y[1,])
    }
    
    c(y)
    
  }
  
  structure(fm, class = "memorized")
  
}

remember <- function(f, combined = TRUE) {
  
  if (!inherits(f, "memorized")) stop("`remember()` only works for memorized functions, see ?remember.", call. = FALSE)
  
  x <- get("x_log", envir = environment(f))
  y <- get("y_log", envir = environment(f))
  
  dfx <- as.data.frame(x)
  names(dfx) <- if (ncol(dfx) == 1) "x" else paste0("x", seq_along(dfx))
  
  dfy <- as.data.frame(y)
  names(dfy) <- if (ncol(dfy) == 1) "y" else paste0("y", seq_along(dfy))
  
  if (combined) cbind(dfx, dfy) else list("x" = dfx, "y" = dfy)
  
}