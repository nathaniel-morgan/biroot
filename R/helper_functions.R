# converts iso data to path structure
iso_to_path <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))
  if (all(lengths == 0)) {
    warning("Zero contours were generated")
    return(tibble0())
  }
  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)
  groups <- paste(group, sprintf("%03d", item_id), sprintf("%03d",
                                                           ids), sep = "-")
  groups <- factor(groups)
  data.frame(level = rep(levels, lengths), x = xs, y = ys,
          piece = as.integer(groups), group = groups, .size = length(xs))
}

#Clones dataframe and attaches
grow_df <- function(grid, x){
  emplist <- list()
  for (i in 1:nrow(grid)) {
    emplist[[i]] <- suppressWarnings(cbind(x, grid[i,,drop = FALSE]))
  }
  Reduce(rbind, emplist)
} 

#clones dataframe and attaches rows or rows and columns with values based
#upon quantiles of selected variables.
class_grid <- function(x, rows = NULL, cols = NULL, quantiles){
  dflist <- list()
  row_quantiles <- factor(quantile(x[[rows]], probs = quantiles))
  qgrid <- data.frame(rows = row_quantiles)
  if( !is.null(cols) ){
    col_quantiles <- factor(quantile(x[[cols]], probs = quantiles))
    qgrid <- expand.grid(rows = row_quantiles,
                             cols = col_quantiles)
  }
  grow_df(qgrid, x)
}

#Thin dataframe to be n long
thin <- function(x, n) {
  x[round(seq(1, nrow(x), length.out = n)),]
}

#Changes values in list x to be thinned data from x_long
thin_list <- function(x, x_long){
  for (i in seq_along(x)) {
    x[[i]][,2:3] <- thin(x_long[[i]], nrow(x[[i]]))
  }
  x
}

#Smooth a dataframe
smooth_one <- function(x, bandwidth = 1/2){
  smoothr::smooth_ksmooth(as.matrix(x[,2:3]), bandwidth = bandwidth)
}

#Seperate iso_data by group smooth it and return smoothed coords
ksmooth_split <- function(iso, bandwidth = 1/2){
  isos <- split(iso, iso$group)
  isos_long <- lapply(isos, smooth_one, bandwidth = bandwidth)
  Reduce(rbind,thin_list(x = isos, x_long = isos_long))
}

#subset dataframe to be nxn around point x, return ratio that is different class.
neighbors <- function(x,df, n, res){
  xx <- as.double(x[1])
  xy <- as.double(x[2])
  mean(x[length(x)]!=subset(df, subset = abs(xx-df$xid) < n & abs(xy-df$yid) < n)$fill)
}

#Specialized sequence writer
seq2R <- function(startx, starty, length, n){
  nei <- c()
  beginy <- max(0,starty-length)
  endy <- min(n, starty+length)
  print(c(beginy,endy))
  for (i in beginy:endy) {
    lbound <- max(1+n*i,startx - length + n*i)
    rbound <- min(n+n*i,startx + length + n*i,n^2)
    if (lbound < rbound){
      nei <- c(nei,seq(lbound, rbound))
    }
  }
  nei
}

#Rewrite to be more efficient
neighbors2 <- function(x, df, n, res){
  xx <- as.double(x[1])
  xy <- as.double(x[2])
  mean(x["fill"]!=df[seq2(xx, xy, n, res),]$fill)
}

#Make isolines generator into helper function for cleaner for loop
form_isolines <- function(data, method, args, mesh, smooth, bandwidth){
  group_col <- which(names(data)=="group")
  names(data)[group_col] <- "fill"
  mod <- do.call(get_classify_train,
                 args = list(data=data,method=method,args=args))
  mesh <- do.call(get_classify_predict,
                  args = list(data=data,mesh=mesh,method=method,args=args,mod=mod))
  breaks <- tail(sort(unique(as.double(mesh$fill))), -1) - 1/2
  names(mesh)[3] <- "z"
  isolines <- xyz_to_isolines(mesh, breaks = breaks)
  isolines <- iso_to_path(isolines)
  if ( smooth ) {
    isolines <- ksmooth_split(isolines, bandwidth)
  }
  isolines
}
