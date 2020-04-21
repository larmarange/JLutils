#' Tidiers for lists returned from chisq.test
#'
#' Tidies objects returned by the \code{\link[stats]{chisq.test}} function.
#' This function is now deprecated. Use \code{augment} method from
#' \code{broom} package
#'
#' @param x list returned by \code{chisq.test} or \code{svychisq}
#' @param ... extra arguments
#' @importFrom broom augment
#' @examples
#' test <- chisq.test(xtabs(Freq ~ Sex + Class, data = as.data.frame(Titanic)))
#' broom::augment(test)
#' @export

tidy_chisq <- function(x, ...) {
  broom::augment(x, ...)
}
