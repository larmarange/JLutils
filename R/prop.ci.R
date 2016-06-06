#' Proportion confidence interval
#' 
#' Provide a convenient wrapper for \code{\link[stats]{prop.test}} to compute 
#' confidence intervals for proportions
#' 
#' @param x a factor vector, a logical vector, a one-dimensional table or a vector of
#'    count of successes (in that case, \code{n} should be provided)
#' @param n a vector of count of trials (\code{x} should be a vector of successes in that case)
#' @param bounds \code{1:2} for lower and upper bounds, \code{1} for lower bounds only, 
#'    \code{2} for upper bound only.
#' @param ... additional parameters used by \code{\link[stats]{prop.test}}
#'  
#' @details 
#'  \code{prop.ci.lower} is a wrapper for \code{prop.ci} with \code{bounds = 1} and
#'  \code{prop.ci.upper} for \code{prop.ci} with \code{bounds = 2}.
#'  
#'  When \code{x} and \code{n} are provided, they should be of same length. Alternativaly,
#'  \code{n} could be a single integer that will be used for each element of \code{x}.
#'  
#'  When \code{x} and \code{n} are provided, if \code{x} contains several elements, 
#'  \code{bounds} should be \code{1} or \code{2} but can't be \code{1:2}. In that scenario,
#'  the returned vector will contain the corresponding lower or the upper confidence interval 
#'  bound for each element of \code{x}.
#'  
#'  The confidence level could be specify with \code{conf.level} (0.95 by default).
#'  
#'  By default, \code{NA} value are removed.
#'  
#' @examples 
#'  if (require(questionr)) {
#'  data(hdv2003)
#'  d <- hdv2003
#'  
#'  freq(d$sport)
#'  prop.ci(d$sport)
#'  prop.ci.lower(d$sport)
#'  prop.ci.upper(d$sport)
#'  prop.ci(d$sport, conf.level = 0.9)
#'  prop.ci(table(d$sport))
#'  prop.ci(d$sport=="Non")
#'  prop.ci(d$sport=="Oui")
#'  
#'  prop.ci.lower(c(1277, 723), n = 2000)
#'  prop.ci.upper(c(1277, 723), n = 2000)
#'  
#'    if (require(data.table)) {
#'      d <- as.data.table(d)
#'      res <- d[,.(freq=.N),by=.(sexe,sport)]
#'      res[, n := sum(freq), by=sexe]
#'      res[, prop := freq/n]
#'      res[, prop.l := prop.ci.lower(freq, n)]
#'      res[, prop.h := prop.ci.upper(freq, n)]
#'      res
#'    }
#'  }
#'  
#' @return a vector with confidence interval
#'  
#' @seealso \code{\link[stats]{prop.test}}
#' 
#' @export
prop.ci <- function (x, n = NULL, bounds = 1:2, ...)
{
  if (length(bounds)>2 | min(bounds)<1 | max(bounds)>2 | !is.numeric(bounds))
    stop("bounds should be 1 (lower), 2 (upper) or 1:2 (both).")
  if (is.null(n) & is.table(x))
    return(prop.test(x, ...)$conf.int[bounds])
  if (is.null(n) & is.logical(x)) {
    x <- na.rm(x)
    return(prop.test(sum(x), length(x), ...)$conf.int[bounds])
  }
  if (is.null(n) & !is.logical(x) & !is.table(x))  
    return(prop.test(table(x), ...)$conf.int[bounds])
  
  if (!is.null(n)) {
    if (length(n)>1 & length(x)!=length(n))
      stop("n should be of length 1 or should have the same length as x.")
    if (length(n)==1) # If only one value of n is provided, this value is used for all elements of x
      n <- rep(n, length.out = length(x))
    if (length(x)>1 & length(bounds)>1)
      stop("When x and n are provided, and if x contains several elements, bounds should be 1 or 2 (but not 1:2).")
    res <- c()
    for (i in 1:length(x))
      res <- c(res, prop.test(x[i], n[i], ...)$conf.int[bounds])
    return(res)
  }
  
}

#' @export
#' @rdname prop.ci
prop.ci.lower <- function (x, n = NULL, ...)
{
  return(prop.ci(x, n, bounds = 1, ...))
}

#' @export
#' @rdname prop.ci
prop.ci.upper <- function (x, n = NULL, ...)
{
  return(prop.ci(x, n, bounds = 2, ...))
}