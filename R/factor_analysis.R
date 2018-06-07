#' Plot of factorial maps with frequency of each point
#'
#' For each point on the factorial plan, display the number of identical
#' observations (i.e. with the same coordinates).
#'
#' @param dfxy a data frame with two coordinates
#' @param xax	column for the x axis
#' @param yax	column for the y axis
#' @param ... additional parameters sent to \code{\link[ade4]{s.value}}
#' @source \url{http://joseph.larmarange.net/?Representer-des-effectifs-dans-le}
#' @seealso \code{\link[ade4]{s.value}}, \code{\link[ade4]{s.label}}
#' @export s.freq
#' @examples
#' if (require(ade4) & require(questionr)) {
#'   data(hdv2003)
#'   acm <- dudi.acm(hdv2003[,c("sexe","sport","bricol","cuisine","cinema")], scannf = FALSE)
#'   s.label(acm$li, clabel = 0, cpoint = 2)
#'   s.freq(acm$li, csize = 0.75)
#' }

s.freq <-
  function(dfxy, xax = 1, yax = 2, ...) {
    if (!requireNamespace("ade4")) {
      stop("ade4 package is required. Please install it.")
    }
    d <- as.data.frame(table(dfxy[c(xax, yax)]))
    d <- d[d$Freq > 0, ]
    d[1] <- as.numeric(as.character(d[[1]]))
    d[2] <- as.numeric(as.character(d[[2]]))
    ade4::s.value(d, d$Freq, ...)
  }
