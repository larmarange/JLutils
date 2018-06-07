#' Inverted cumulative sum
#'
#' This is a decreasing version of \code{cumsum}.
#'
#' @param x numerical vector
#' @export
#' @examples
#' cumsum(1:10)
#' inv_cumsum(1:10)

inv_cumsum <- function(x) {
  sum(x) - cumsum(x) + x
}
