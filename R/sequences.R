#' Index plot of sequences oderred according to a dendrogram
#' @source \url{http://joseph.larmarange.net/?Representer-un-tapis-de-sequences}
#' @export seq.heatmap

seq.heatmap <- function (seq, tree, with.missing=FALSE, ...) {
	if (!require(TraMineR)) stop("You need to install the TraMineR package.")
	if (class(tree)!="dendrogram") tree <- as.dendrogram(tree)
	mat <- seq
	for (i in 1:length(seq)){
		mat[mat[,i]=="%",i] <- NA
		mat[,i] <- as.numeric(mat[,i])
	}
	mat <- as.matrix(mat)
	col <- attr(seq,"cpal")
	if (with.missing) col <- c(col,attr(seq,"missing.color"))
	heatmap(mat, tree, NA,  na.rm=FALSE, col=col, scale="none", labRow=NA, ...)	
}