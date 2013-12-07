#' Optimal partition based on the higher relative loss of inertia
#' @source \url{http://joseph.larmarange.net/?Ou-couper-un-dendrogramme}
#' @export best.cutree

best.cutree <- 
function(hc, min=3, max=20, loss=FALSE, graph=FALSE, ...){
	if (class(hc)!="hclust") hc <- as.hclust(hc)
	max <- min(max, length(hc$height))
	inert.gain <- rev(hc$height)
	intra <- rev(cumsum(rev(inert.gain)))
	relative.loss = intra[min:(max)]/intra[(min - 1):(max - 1)]
	best = which.min(relative.loss)
	names(relative.loss) <- min:max
	if (graph) {
		temp <- relative.loss
		temp[best] <- NA
		best2 <- which.min(temp)
		pch <- rep(1, max-min+1)
		pch[best] <- 16
		pch[best2] <- 21
		plot(min:max, relative.loss, pch=pch, bg="grey75", ...)
	} else {
		if (loss)
			relative.loss
		else
			best + min - 1
	}
}