#' Round values while preserve their rounded sum in R
#' 
#' In general, the sum of rounded numbers (e.g., using the 
#' \code{base::round} function) is not the same as their rounded sum.
#' This solution applies the following algorithm
#' \enumerate{
#'    \item Round down to the specified number of decimal places
#'    \item Order numbers by their remainder values
#'    \item Increment the specified decimal place of values with ‘k’ largest remainders, 
#'    where ‘k’ is the number of values that must be incremented to preserve their rounded sum
#' }
#' @source \url{https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/} and
#' \url{http://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum}
#' @examples 
#' sum(c(0.333, 0.333, 0.334))
#' round(c(0.333, 0.333, 0.334), 2)
#' sum(round(c(0.333, 0.333, 0.334), 2))
#' round_preserve_sum(c(0.333, 0.333, 0.334), 2)
#' sum(round_preserve_sum(c(0.333, 0.333, 0.334), 2))
#' @export
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}