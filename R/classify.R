#' Estimated class of each square of the raster 
#' 
#' Perform model estimation and compute the estimated class of every point
#' on a grid. `geom_classify` draws filled regions.
#'
#' @section Aesthetics: `geom_classify()` understands the following aesthetics
#'   (required aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - **fill**
#'   - alpha
#'   - color
#'   - group
#'   - linetype
#'   - linewidth
#'
#' @inheritParams ggplot2::geom_raster
#' @param method Density estimator to use, accepts character vector:
#'   `"lda"`,`"qda"`, `"randomForest"`, or `"knn"`.
#'   Alternatively accepts functions  along with a list containing arguments.
#' @param res resolution of the raster which will be formed of a resxres grid.
#' @param fade If set to true the alpha will fade as it moves away from borders
#' between classes
#' @param mesh A custom mesh can be utilized with mesh and the classification
#' will be computed over it.
#' @param xlim,ylim Range to compute and draw regions. If `NULL`, defaults to
#'   range of data.
#' @name geom_classify
#'
#'
#' @examples
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
#' set.seed(17)
#' df <- rdata(100, n_groups = 5, radius = 2.25)
#'   
#' ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c))
#'   
#' ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify() # defaults to method = "lda"
#'   
#' ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "qda")
#'   
#' ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "knn")
#'   
#' ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "knn", res = 1e3)
#' 
#' # Plot LDA and QDA methods
#' 
#' 
#' p_lda <- ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "lda") +
#'   ggtitle("Linear Discriminant Analysis")
#' 
#' p_qda <- ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "qda") +
#'   ggtitle("Quadratic Discriminant Analysis")
#'   
#' p_knn <- ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "knn") +
#'   ggtitle("K Nearest Neighbors")
#'  
#' p_rf <- ggplot(data = df, aes(x, y, fill = c)) +
#'   geom_point_fill(aes(fill = c)) +
#'   geom_classify(method = "randomForest") +
#'   ggtitle("Random Forest")
#'   
#' library(patchwork)
#' 
#' (p_lda + p_qda) / (p_knn + p_rf) + plot_layout(guides = "collect")
#'   
#' # Plot with lower resolution
#' 
#' ggplot(data = df, aes(x, y, fill = c))+
#'   geom_point_fill(aes(fill = c))+
#'   geom_classify(res = 1e1,method = "qda")
#'   
#' 
#' # Plot with basic tidymodels methods
#' 
#' rdata(100, 5, 3) |> ggplot(aes(x, y, fill = c))+
#'  geom_point_fill(aes(fill=c))+
#'  geom_classify()+
#'  coord_equal()
#'   
#' qda_mod <- discrim_quad() %>%
#'   set_mode("classification") %>%
#'   set_engine("MASS")
#' 
#' rf_mod <- rand_forest(mode="classification") |> 
#'   set_engine("ranger",importance = "impurity")
#' 
#' ggplot(data = df1, aes(x, y, fill = c))+
#'   geom_point_fill(aes(fill = c))+
#'   geom_classify(method = rf_mod)
#'   
#' ggplot(data = df1, aes(x, y, fill = c))+
#'   geom_point_fill(aes(fill = c))+
#'   geom_classify(method = qda_mod)
#'   
#' # Plot with custom function
#' 
#' custom_method <- function(data,mesh)  {
#'   mesh$fill <- factor((mesh$x^2+mesh$y^2 < 2))
#'   return(mesh)
#' }
#' 
#' ggplot(data = df, aes(x, y, fill = c))+
#'   geom_point_fill(aes(fill = c))+
#'   geom_classify(method = custom_method)+
#'   coord_equal()
#' 
#'   
#'   
#'   
#'   

#' @rdname geom_classify
#' @format NULL
#' @usage NULL
#' @export
cpt_class <- function(data, scales, mesh = 1, res = 101, method,
                      fade = FALSE, args=NULL, mod = NULL,
                      invert = FALSE, neighborhood = 10) {
  # Check for custom mesh
  if (sum(mesh) == 1){
    # form a mesh and predict class for each
    xrange <- scales$x$range$range
    yrange <- scales$y$range$range
    mesh <- expand.grid(x=seq(xrange[1],xrange[2], length.out = res),
                        y=seq(yrange[1],yrange[2], length.out = res))
    if ( is.null(mod) ){
      mod <- do.call(get_classify_train,
                     args = list(data=data,method=method,args=args)) 
    }
    mesh <- do.call(get_classify_predict,
            args = list(data=data,mesh=mesh,method=method,args=args,mod=mod))
    if( fade ){
      meshid <- cbind(expand.grid(xid = 1:res, yid = 1:res),mesh)
      purity <- 1-apply(meshid, MARGIN = 1, FUN = neighbors2,
                      df = meshid, n = neighborhood, res = res)
      mesh$alpha <- 1-purity
      mesh$purity <- purity
      if( invert ) mesh$alpha <- purity
    }
    mesh$density <- apply(as.data.frame(lapply(split(data, data$fill),
                                         function(x) c(MASS::kde2d(x$x, x$y, n=res,
                                                                   lims = c(xrange, yrange))[["z"]]))),
                          MARGIN = 1,max)
    cbind(mesh, class = mesh$fill)
  } else  {
    mesh <- do.call(get_classify,args = list(data=data,mesh=mesh,method=method,args=args))
    if( fade ){
      meshid <- cbind(expand.grid(xid = 1:res, yid = 1:res),mesh)
      mesh$alpha <- apply(meshid, MARGIN = 1, FUN = neighbors2,
                          df = meshid, n = 10, res = res)
      if( invert ) mesh$alpha <- 1-mesh$alpha
    }
    cbind(mesh, class = mesh$fill)
  }
}




cpt_par = function(data, params, res=101) {
  # form a mesh and predict class for each
  if( is.null(params$method) ){
    params$method <- "lda"
  }
  return(params)
}





cpt_data <- function(data,params) {
  return(data)
}





Statclassify <- ggproto("Statclassify", Stat,
                            setup_params = cpt_par,
                            setup_data = cpt_data,
                            compute_panel = cpt_class,
                            required_aes = c("x","y"),
                            #default_aes = aes(fill = ..fill..)
)





#' @rdname geom_classify
#' @export
stat_classify <- function(mapping = NULL, data = NULL, geom = Geomclassify,
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, res = NULL , method = NULL,
                          fade = FALSE, invert = FALSE, neighborhood = NULL, ...) {
  layer(
    stat = Statclassify, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(res = res, na.rm = na.rm, method = method,
                  fade = fade, neighborhood = neighborhood, invert = invert, ...)    
  )
}





Geomclassify <- ggproto("Geomclassify", GeomRaster,
                        default_aes = aes(
                          alpha = 0.35,
                          fill = "gray"
                        )
)





#' @rdname geom_classify
#' @export
geom_classify <- function(mapping = NULL, data = NULL, stat = Statclassify,
                             position = "identity", na.rm = FALSE, 
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = Geomclassify, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}