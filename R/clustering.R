#' Optimal partition based on the higher relative loss of inertia
#' 
#' This function calculates the best partition to cut a dendrogram based on
#' the higher relative loss of inertia criteria. This criteria was originaly
#' proposed by the \code{\link[FactoMineR]{HCPC}} function of the package
#' \code{FactoMineR}.
#' 
#' @param hc a clustering tree (an object of class \code{"hclust"}, \code{"dendrogram"} or \code{"agnes"})
#' @param min the minimum number of classes
#' @param max the maximum number of classes
#' @param loss if \code{TRUE}, will return the relative loss of inertia of each partition instead of the best partition
#' @param graph if \code{TRUE}, will plot the relative loss of inertia of each partition, the best partition being indicated in black and the second best in grey
#' @param ... additional arguments sent to \code{plot} (if \code{graph = TRUE})
#' @seealso \code{\link[FactoMineR]{HCPC}}
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' best.cutree(hc)
#' best.cutree(hc, loss = TRUE)
#' best.cutree(hc, graph = TRUE)
#' best.cutree(hc, graph = TRUE, min = 6, max = 15)
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
	}
	if (loss)
		relative.loss
	else
		best + min - 1
}