\name{seq.heatmap}
\alias{seq.heatmap}
\title{Index plot of sequences orderred according to a dendrogram}
\source{
  \url{http://joseph.larmarange.net/?Representer-un-tapis-de-sequences}
}
\usage{
  seq.heatmap(seq, tree, with.missing = FALSE, ...)
}
\arguments{
  \item{seq}{a state sequence object created with the
  \code{\link[TraMineR]{seqdef}} function}

  \item{tree}{a dendrogram of the sequences (an object of
  class \code{\link{hclust}}, \code{\link{dendrogram}} or
  \code{\link{agnes}})}

  \item{with.missing}{is there a 'missing value' state in
  the sequences?}
}
\description{
  Index plot of state sequences. Sequences are ordered
  accoring to the specified dendrogram. The dendrogram is
  also plotted on the side of the index plot.
}
\examples{
if (require(TraMineR)) {
  data(mvad)
  mvad.seq <- seqdef(mvad[,17:86])
  mvad.lcs <- seqdist(mvad.seq, method = "LCS")
  mvad.hc <- agnes(mvad.lcs, method = "ward")
  seq.heatmap(mvad.seq, mvad.hc)
  seqIplot(mvad.seq, sortv = cutree.order(mvad.hc, nrow(mvad.seq)))
}
}
\seealso{
  \code{\link[TraMineR]{seqiplot}}
}

