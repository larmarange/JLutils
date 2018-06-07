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

seq_heatmap <- function(seq, tree, with.missing = FALSE, ...) {
  if (!requireNamespace("TraMineR")) {
    stop("TraMineR package is required. Please install it.")
  }
  if (!inherits(seq, "stslist")) stop("seq should be a stslist object, see ?seqdef.")
  if (!inherits(tree, "dendrogram")) tree <- as.dendrogram(tree)
  mat <- seq
  for (i in 1:length(seq)) {
    mat[mat[, i] == "%", i] <- NA
    mat[, i] <- as.numeric(mat[, i])
  }
  mat <- as.matrix(mat)
  col <- attr(seq, "cpal")
  if (with.missing) col <- c(col, attr(seq, "missing.color"))
  heatmap(mat, tree, NA, na.rm = FALSE, col = col, scale = "none", labRow = NA, ...)
}

#' Create a riverplot from a sequence object
#'
#' @param seq a stslist object (typically produced with \code{\link[TraMineR]{seqdef}})
#'
#' @return a river plot object that can be plotted with \code{\link[riverplot]{riverplot}}
#'
#' @export

seq_makeRiver <- function(seq) {
  if (!requireNamespace("TraMineR")) {
    stop("TraMineR package is required. Please install it.")
  }
  if (!requireNamespace("riverplot")) {
    stop("riverplot package is required. Please install it.")
  }
  if (!inherits(seq, "stslist")) stop("seq should be a stslist object, see ?seqdef.")

  alphabet <- attr(seq, "alphabet")
  cpal <- attr(seq, "cpal")
  n.states <- ncol(seq)

  nodes <- data.frame()
  for (i in 1:n.states) {
    for (j in 1:length(alphabet)) {
      nodes <- rbind(nodes, data.frame(
        ID = paste0(i, "-", alphabet[j]),
        x = i,
        col = cpal[j],
        labels = sum(seq[[i]] == alphabet[j], na.rm = TRUE)
      ))
    }
  }

  nodes$ID <- as.character(nodes$ID)
  nodes$col <- as.character(nodes$col)
  nodes <- nodes[nodes$labels > 0, ]

  edges <- data.frame()
  for (i in 1:(n.states - 1)) {
    for (j1 in 1:length(alphabet)) {
      for (j2 in 1:length(alphabet)) {
        edges <- rbind(edges, data.frame(
          N1 = paste0(i, "-", alphabet[j1]),
          N2 = paste0(i + 1, "-", alphabet[j2]),
          Value = sum(seq[[i]] == alphabet[j1] & seq[[i + 1]] == alphabet[j2], na.rm = TRUE)
        ))
      }
    }
  }

  edges$N1 <- as.character(edges$N1)
  edges$N2 <- as.character(edges$N2)
  edges <- edges[edges$Value > 0, ]

  return(riverplot::makeRiver(nodes, edges))
}
