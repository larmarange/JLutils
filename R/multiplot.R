#' Multiple plot
#' 
#' Renders multiple ggplot plots in one image
#' 
#' @param ... ggplot objects
#' @param plotlist a list of ggplot objects
#' @param cols number of columns in layout
#' @param layout a matrix specifying the layout. if present, \code{cols} is ignored
#' 
#' @note If the layout is something like \code{matrix(c(1,2,3,3), nrow=2, byrow=TRUE)}, 
#' then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all 
#' the way across the bottom.
#' 
#' @references Winston Chang, \emph{Cookbook for R}, 
#' \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#' 
#' @examples 
#' require(ggplot2)
#' p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p2 <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
#' p3 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_violin()
#' p4 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
#' 
#' multiplot(p1, p2, p3, p4)
#' multiplot(p1, p2, p3, p4, cols = 2)
#' multiplot(p1, p2, p3, layout = matrix(c(1,2,3,3), nrow=2))
#' multiplot(p1, p2, p3, layout = matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
#' 
#' @export

# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist = NULL, cols =1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}