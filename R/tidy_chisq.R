#' Tidiers for lists returned from chisq.test
#' 
#' Tidies objects returned by the \code{\link[stats]{chisq.test}} function.
#' 
#' @param x list returned by \code{chisq.test} or \code{svychisq}
#' @examples 
#' test <- chisq.test(xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic)))
#' tidy_chisq(test)
#' @export

tidy_chisq <- function(x, ...) {
  if (!all(c("observed", "expected", "residuals", "stdres") %in% names(x)))
    stop("x should have observed, expected, residuals and stdres entries.")
  
  d <- length(dim(x$observed))
  res <- as.data.frame.table(x$observed)
  names(res)[d+1] <- "observed"
  
  res <- cbind(res, prop = as.data.frame.table(prop.table(x$observed))[[d+1]])
  if (d == 2) {
    res <- cbind(res, row.prop = as.data.frame.table(prop.table(x$observed, 1))[[d+1]])
    res <- cbind(res, col.prop = as.data.frame.table(prop.table(x$observed, 2))[[d+1]])
  }
  
  res <- cbind(res, expected = as.data.frame.table(x$expected)[[d+1]])
  res <- cbind(res, residuals = as.data.frame.table(x$residuals)[[d+1]])
  res <- cbind(res, stdres = as.data.frame.table(x$stdres)[[d+1]])
  res
}