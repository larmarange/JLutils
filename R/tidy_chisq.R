#' Tidiers for lists returned from chisq.test
#' 
#' Tidies objects returned by the \code{\link[stats]{chisq.test}} function.
#' 
#' @param x list returned by \code{chisq.test} or \code{svychisq}
#' @param ... extra arguments
#' @examples 
#' test <- chisq.test(xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic)))
#' tidy_chisq(test)
#' @export

tidy_chisq <- function(x, ...) {
  if (!all(c("observed", "expected", "residuals", "stdres") %in% names(x)))
    stop("x should have observed, expected, residuals and stdres entries.")
  
  d <- length(dimnames(as.table(x$observed)))
  ret <- as.data.frame(as.table(x$observed))
  names(ret)[d+1] <- "observed"
  
  ret <- cbind(ret, prop = as.data.frame(prop.table(as.table(x$observed)))[[d+1]])
  if (d == 2) {
    ret <- cbind(ret, row.prop = as.data.frame(prop.table(as.table(x$observed, 1)))[[d+1]])
    ret <- cbind(ret, col.prop = as.data.frame(prop.table(as.table(x$observed, 2)))[[d+1]])
  }
  
  ret <- cbind(ret, expected = as.data.frame(as.table(x$expected))[[d+1]])
  ret <- cbind(ret, residuals = as.data.frame(as.table(x$residuals))[[d+1]])
  ret <- cbind(ret, stdres = as.data.frame(as.table(x$stdres))[[d+1]])
  ret
}