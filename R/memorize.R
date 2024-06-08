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