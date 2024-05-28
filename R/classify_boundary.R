#' Estimated class of plotted region and draw the decision boundaries between 
#' different classes.
#' 
#' Perform model estimation and compute the estimated class of every point
#' on a grid. `geom_classify_boundary` draws the decision boundaries.
#'
#' @section Aesthetics: `geom_classify_boundary()` understands the following aesthetics (required
#'   aesthetics are in bold). Fill can be used in place of group.:
#'
#'   - **x**
#'   - **y**
#'   - **group**
#'   - alpha
#'   - color
#'   - linetype
#'   - linewidth
#'
#' @inheritParams ggplot2::geom_path
#' @param method Density estimator to use, accepts character vector:
#'   `"lda"`,`"qda"`, `"randomForest"`, or `"knn"`.
#'   Alternatively accepts functions  along with a list containing arguments.
#' @param res resolution of the raster which will be formed of a res x res grid.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to
#'   range of data.
#' @param smooth If set to true a guassian kernel smoother will be applied to
#' the boundary line.
#' @param smooth sets the bandwidth for the kernel density smoother. Defaults to 1/2.
#' @name geom_classify_boundary
#'
#'
#' @examples
#' # Basic simulated data with bivariate normal data and various methods
#' 
#' # Simulate Data
#' set.seed(17)
#' si <- 3/4
#' n <- 25
#' 
#' df <- data.frame(x = rnorm(n,sd=si),y=rnorm(n,sd=si)+2,class = factor("A")) |>
#'   rbind(data.frame(x = rnorm(n,sd=2*si)+2,y=rnorm(n,sd=2*si)-1,class = factor("B"))) |>
#'   rbind(data.frame(x = rnorm(n,sd=1/2*si)+2,y=rnorm(n,sd=1/2*si)+1,class = factor("C")))
#' 
#' # Plot LDA and QDA methods
#' 
#' 
#' p_lda <- ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = "lda")+
#'   ggtitle("Linear Discriminant Analysis")
#' 
#' p_qda <- ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = "qda")+
#'   ggtitle("Quadratic Discriminant Analysis")
#'   
#' p_knn <- ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = "knn")+
#'   ggtitle("K Nearest Neighbors")
#'  
#' p_rf <- ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = "randomForest")+
#'   ggtitle("Random Forest")
#'   
#' library(patchwork)
#' 
#' (p_lda + p_qda) / (p_knn + p_rf) + plot_layout(guides = "collect")
#'   
#' # Plot with lower resolution
#' 
#' ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(res = 1e1,method = "qda")
#'   
#' rdata <- function(n, n_groups = 3, radius = 3) {
#'   list_of_dfs <- lapply(0:(n_groups-1), function(k) {
#'     mu <- c(cos(2*k*pi/n_groups), sin(2*k*pi/n_groups))
#'     m <- MASS::mvrnorm(n, radius*mu, diag(2))
#'     structure(data.frame(m, as.character(k)), names = c("x", "y", "c"))
#'   })
#'   do.call("rbind", list_of_dfs)
#' }
#' 
#' # Plot with basic tidymodels methods
#' 
#' rdata(100, 5, 3) |> ggplot(aes(x, y, group = c))+
#'  geom_point_fill(aes(fill=c))+
#'  geom_classify_boundary()+
#'  coord_equal()
#'   
#' qda_mod <- discrim_quad() %>%
#'   set_mode("classification") %>%
#'   set_engine("MASS")
#' 
#' rf_mod <- rand_forest(mode="classification") |> 
#'   set_engine("ranger",importance = "impurity")
#' 
#' ggplot(data=df1, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = rf_mod)
#'   
#' ggplot(data=df1, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = qda_mod)
#'   
#' # Plot with custom function
#' 
#' custom_method <- function(data,mesh)  {
#'   mesh$fill <- factor((mesh$x^2+mesh$y^2 < 2))
#'   return(mesh)
#' }
#' 
#' ggplot(data=df, aes(x, y, group = class))+
#'   geom_point_fill(aes(fill = class))+
#'   geom_classify_boundary(method = custom_method)+
#'   coord_equal()
#' 
#'   
#'   
#'   
#'   

#' @rdname geom_classify_boundary
#' @format NULL
#' @usage NULL
#' @export

cpt_class_bd <- function(data, scales, mesh = 1, res = 101, method,
                         args=NULL, smooth = FALSE, bandwidth = 1/2) {
  # Check for custom mesh
  if ( sum(mesh) == 1 ){
    # form a mesh and predict class for each
    xrange <- scales$x$range$range
    yrange <- scales$y$range$range
    mesh <- expand.grid(x=seq(xrange[1], xrange[2], length.out = res),
                        y=seq(yrange[1], yrange[2], length.out = res))
    if ( !is.null(data$colour) & !is.null(data$linetype) ){
      emptylist <- list()
      ctable <- unique(data$colour)
      ltable <- unique(data$linetype)
      clgrid <- expand.grid(colour = ctable, linetype = ltable)
      for (i in 1:nrow(clgrid)) {
        emptylist[[i]] <- cbind(form_isolines(data = subset(data,
                                colour == clgrid$colour[i] & linetype == clgrid$linetype[i]),
                                              method = method, args = args,
                                              mesh = mesh, smooth = smooth,
                                              bandwidth = bandwidth),
                                colour = clgrid[[i,1]], linetype = clgrid[[i,2]])
        emptylist[[i]]$group <- factor(paste0(emptylist[[i]]$group,"-",
                                              emptylist[[i]]$colour,"-",
                                              emptylist[[i]]$linetype))
      }
      Reduce(rbind, emptylist)
    } else if ( !is.null(data$colour) ){
      emptylist <- list()
      ctable <- unique(data$colour)
      for (i in 1:length(ctable)) {
        emptylist[[i]] <- cbind(form_isolines(data = subset(data, colour == ctable[i]),
                                              method = method, args = args,
                                              mesh = mesh, smooth = smooth,
                                              bandwidth = bandwidth),
                                colour = ctable[i])
        emptylist[[i]]$group <- factor(paste0(emptylist[[i]]$group,"-",emptylist[[i]]$colour))
      }
      Reduce(rbind, emptylist)
    } else if ( !is.null(data$linetype) ) {
      emptylist <- list()
      ctable <- unique(data$linetype)
      for (i in 1:length(ctable)) {
        emptylist[[i]] <- cbind(form_isolines(data = subset(data, linetype == ctable[i]),
                                              method = method, args = args,
                                              mesh = mesh, smooth = smooth,
                                              bandwidth = bandwidth),
                                linetype = ctable[i])
        emptylist[[i]]$group <- factor(paste0(emptylist[[i]]$group,"-",emptylist[[i]]$linetype))
      }
      Reduce(rbind, emptylist)
    }
    else form_isolines(data = data, method = method, args = args,
                       mesh = mesh, smooth = smooth, bandwidth = bandwidth)
  } else  {
    group_col <- which(names(data)=="group")
    names(data)[group_col] <- "fill"
    print(data)
    mod <- do.call(get_classify_train,
                   args = list(data=data,method=method,args=args))
    do.call(get_classify_predict,
            args = list(data=data,mesh=mesh,method=method,args=args,mod=mod))
  }
}




cpt_par_bd = function(data, params, res=101) {
  # form a mesh and predict class for each
  if( is.null(params$method) ){
    params$method <- "lda"
  }
  return(params)
}





cpt_data_bd <- function(data,params) {
  return(data)
}





StatClassifyBoundary <- ggproto("StatClassifyBoundary", Stat,
                        setup_params = cpt_par_bd,
                        setup_data = cpt_data_bd,
                        compute_panel = cpt_class_bd,
                        required_aes = c("x","y")
)





#' @rdname geom_classify_boundary
#' @export
stat_classify_boundary <- function(mapping = NULL, data = NULL, geom = GeomPath,
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, res = NULL , method = NULL, grid = NULL, ...) {
  layer(
    stat = StatClassifyBoundary, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(res = res, na.rm = na.rm, method = method,
                  smooth = smooth, bandwidth = bandwidth, ...)    
  )
}


#' @rdname geom_classify_boundary
#' @export
geom_classify_boundary <- function(mapping = NULL, data = NULL, stat = StatClassifyBoundary,
                          position = "identity", na.rm = FALSE, 
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomPath, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}