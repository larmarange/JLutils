#' Compute automatically the index for grouping rows with kableExtra 
#' 
#' When using \code{group_rows} function from \code{kableExtra}, you can provide a names vector providing 
#' the number of rows to group together. \code{auto_index_group_rows} compute that index based on a vector
#' by counting the successive identical values of that vector.
#' 
#' @param x a vector
#' @export
#' @examples 
#' x <- c("a", "a", "a", "b", "b", "a", "c", "c")
#' auto_index_group_rows(x)

auto_index_group_rows <- function(x) {
  lx <- dplyr::lead(x)
  
  index <- c()
  name <- c()
  n <- 0
  for (i in 1:length(x)) {
    n <- n + 1
    if (!identical(x[i], lx[i]) | i == length(x)) {
      index <- c(index, n)
      name <- c(name, x[i])
      n <- 0
    }
  }
  names(index) <- name
  index
}