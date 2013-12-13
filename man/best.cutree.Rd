\name{best.cutree}
\alias{best.cutree}
\title{Optimal partition based on the higher relative loss of inertia}
\usage{
  best.cutree(hc, min = 3, max = 20, loss = FALSE,
    graph = FALSE, ...)
}
\arguments{
  \item{hc}{a clustering tree (an object of class
  \code{"hclust"}, \code{"dendrogram"} or \code{"agnes"})}

  \item{min}{the minimum number of classes}

  \item{max}{the maximum number of classes}

  \item{loss}{if \code{TRUE}, will return the relative loss
  of inertia of each partition instead of the best
  partition}

  \item{graph}{if \code{TRUE}, will plot the relative loss
  of inertia of each partition, the best partition being
  indicated in black and the second best in grey}

  \item{...}{additional arguments sent to \code{plot} (if
  \code{graph = TRUE})}
}
\description{
  This function calculates the best partition to cut a
  dendrogram based on the higher relative loss of inertia
  criteria. This criteria was originaly proposed by the
  \code{\link[FactoMineR]{HCPC}} function of the package
  \code{FactoMineR}.
}
\examples{
hc <- hclust(dist(USArrests), "ave")
best.cutree(hc)
best.cutree(hc, loss = TRUE)
best.cutree(hc, graph = TRUE)
best.cutree(hc, graph = TRUE, min = 6, max = 15)
}
\seealso{
  \code{\link[FactoMineR]{HCPC}}
}

