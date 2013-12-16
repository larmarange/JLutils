#' Index plot of sequences orderred according to a dendrogram
#' 
#' Index plot of state sequences. Sequences are ordered accoring to the specified
#' dendrogram. The dendrogram is also plotted on the side of the index plot.
#' 
#' @param seq a state sequence object created with the \code{\link[TraMineR]{seqdef}} function
#' @param tree a dendrogram of the sequences (an object of class \code{\link{hclust}}, \code{\link{dendrogram}} or \code{\link{agnes}})
#' @param with.missing is there a 'missing value' state in the sequences?
#' @param ... additional parameters sent to \code{\link{heatmap}}
#' @source \url{http://joseph.larmarange.net/?Representer-un-tapis-de-sequences}
#' @seealso \code{\link[TraMineR]{seqIplot}}
#' @export seq_heatmap
#' @examples
#' if (require(TraMineR) & require(cluster)) {
#'   data(mvad)
#'   mvad.seq <- seqdef(mvad[,17:86])
#'   mvad.lcs <- seqdist(mvad.seq, method = "LCS")
#'   mvad.hc <- agnes(mvad.lcs, method = "ward")
#'   seq_heatmap(mvad.seq, mvad.hc)
#'   seqIplot(mvad.seq, sortv = cutree.order(mvad.hc, nrow(mvad.seq)))
#' }

seq_heatmap <- function (seq, tree, with.missing = FALSE, ...) {
	if (!require(TraMineR)) stop("You need to install the TraMineR package.")
	if (!inherits(tree, "dendrogram")) tree <- as.dendrogram(tree)
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