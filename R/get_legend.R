#' Extract the legend of a ggplot2
#' 
#' @param p a ggplot with a legend
#' 
#' @note Could be combined with \code{\link{multiplot}} (see examples).
#' 
#' @references Adapted from \url{http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot}
#' @seealso \url{https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}
#' 
#' @examples 
#' require(ggplot2)
#' p1 <- ggplot(iris) + 
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Species) + 
#'   geom_point() + 
#'   theme(legend.position = "bottom") 
#' p2 <- ggplot(iris) + 
#' aes(x = Petal.Length, y = Petal.Width, color = Species) + 
#' geom_point()
#' 
#' # get legend from p1
#' l <- get_legend(p1)
#' 
#' # remove legends
#' p1 <- p1 + theme(legend.position = "none")
#' p2 <- p2 + theme(legend.position = "none")
#' 
#' # Combining
#' multiplot(p1, p2, l, heights = c(2, 2, 1))
#' 
#' @export

get_legend<-function(p){
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}