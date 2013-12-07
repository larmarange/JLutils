#' Display frequencies on the factorial plan
#' @source \url{http://joseph.larmarange.net/?Representer-des-effectifs-dans-le}
#' @export s.freq

s.freq <-
	function(dfxy, xax=1, yax=2, ...)
	{
		if (!require(ade4)) stop("You need to install the ade4 package.")
		d <- as.data.frame(table(dfxy[c(xax,yax)]))
		d <- d[d$Freq>0,]
		d[1] <- as.numeric(as.character(d[[1]]))
		d[2] <- as.numeric(as.character(d[[2]]))
		s.value(d,d$Freq, ...)
	}