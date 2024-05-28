#' Plot multiple different methods for `geom_classify` at once utilizing patchwork
#' 
#' Perform model estimation and compute the estimated class of every point
#' on a grid. `geom_classify` draws filled regions.
#'
#' @inheritParams ggplot2::geom_raster
#' @param baseplot the core ggplot object added to by each method
#' @param methods a list of methods to be plotted.
#' @param titles a vector of strings that will be used for plot titles in the same
#' order that methods are selected.
#' @param boundary if set to true `geom_classify_boundary` will be used instead.
#' @name classify_multi

#' @rdname classify_multi
#' @format NULL
#' @usage NULL
#' @export
classify_multi <- function(base_plot, methods, titles = NULL, boundary = FALSE, ...){
  class_geom <- "geom_classify"
  if ( boundary ) class_geom <- "geom_classify_boundary"
  for (i in seq_along(methods)) {
    assign(
      paste0("p", i),
      base_plot + 
        do.call(class_geom, args = list(method = methods[[i]], ...)) +
        ggtitle(titles[i]) +
        theme(plot.title = element_text(hjust = 0.5)), 
      parent.frame()
    )
  }
  p <- lapply(as.list(paste0("p", seq_along(methods))), get)
  wrap_plots(p) + plot_layout(guide = "collect")
}

#Look into mget

#' Compute n principle components for a data frame and append them as
#' new columns. Does not utilize the class variable you are interested
#' in plotting for.
#'
#' @param x data frame to compute principal components for. 
#' @param class The name of the class variable you don't want included in the PC's.
#' @param scale Whether to center and scale the data before Computing PC's.
#' @param exclude character vector of any other columns to excluded from PC's.
#' @param n The number of PC's to compute and append.
#' @name pca

#' @rdname pca
#' @format NULL
#' @usage NULL
#' @export
pca <- function(x, class, scale = FALSE, exclude = NULL, n = 2){
  matx <- x[which(!names(x) %in% exclude)]
  names(matx)[which(names(matx) == class)] <- "class"
  if ( scale ) matx <- scale(model.matrix(class~.+0, data = matx))
  else matx <- model.matrix(class~.+0, data = matx)
  eigs <- eigen(t(matx) %*% matx)
  vecs <- as.data.frame(matx %*% eigs$vectors[,1:n])
  names(vecs) <- paste0("pca",1:n)
  cbind(x,vecs)
}




