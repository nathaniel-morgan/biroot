#' Plot points that map with the fill aesthetic
#' 
#'
#' @section Aesthetics: `geom_point_fill()` understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - fill
#'   - alpha
#'   - color
#'   - group
#'   - size
#'   - stroke
#'   - shape
#'
#' @inheritParams ggplot2::geom_point
#' @name geom_point_fill
#'
#'
#' @examples
#' # Basic simulated data plotted
#' 
#' set.seed(17)
#' data.frame(x=rnorm(100),y=rnorm(100),count=factor(rpois(100,1))) |> 
#' ggplot(aes(x,y,fill=count))+
#' geom_point_fill()
#' 

#' @rdname geom_point_fill
#' @format NULL
#' @usage NULL
#' @export
GeomPointFill <- ggproto(
  "GeomPointFill",
  GeomPoint,
  
  default_aes = aes(
    shape = 21,
    colour = "black",
    size = 2,
    fill = "grey75",
    alpha = NA,
    stroke = .5
  )
  
)

#' @rdname geom_point_fill
#' @export
geom_point_fill <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointFill,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}