#' Easy ggplot with survey object
#' 
#' A function to facilitate ggplot graphs using a survey object.
#' It will initiate a ggplot and map survey weights to the 
#' corresponding aesthetic.
#' 
#' Graphs will be correct as long as only weights are required
#' to compute the graph. However, statistic or geometry requiring
#' correct variance computation (like \code{geom_smooth()}) will 
#' be statistically incorrect.
#' 
#' @param design A survey design object
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' 
#' @export
#' @examples 
#' if (require(survey)) {
#'   data(api)
#'   dstrat <- svydesign(
#'     id = ~1, strata = ~stype, 
#'     weights = ~pw, data=apistrat, 
#'     fpc = ~fpc
#'   )
#'   ggsurvey(dstrat) + 
#'     aes(x = cnum, y = dnum) + 
#'     geom_count()
#'  
#'   d <- as.data.frame(Titanic)
#'   dw <- svydesign(ids = ~1, weights=~Freq, data = d)
#'   ggsurvey(dw) + aes(x = Class, fill = Survived) + geom_bar(position = "fill")
#' }
ggsurvey <- function(design = NULL, mapping = aes(), ...) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("msSurv package is required.")
  }
  data <- design$variables
  data$.weights <- weights(design)
  mapping$weight <- aes(weight = .weights)$weight
  
  ggplot(data, mapping, ...)
}

