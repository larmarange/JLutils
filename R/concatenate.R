#' Concatenate operator
#' 
#' The \code{\%p\%} operator is a shortcut for \code{\link[base]{paste0}}.
#' @param x a first vector to paste
#' @param y a second vector to paste
#' @export
#' @examples 
#' "abc" %p% "def"
#' "v_" %p% 1:3
#' c("A", "B", "C") %p% 1:3
`%p%` <- function(x,y){paste0(x,y)}