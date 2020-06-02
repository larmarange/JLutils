#' Deprecated functions
#' 
#' These functions are deprecated.
#' 
#' \code{tidy.clm}, \code{tidy.clmm} and \code{tidy.svyolr} are now part of \pkg{broom}
#'  and have been removed from \pkg{JLutils}.
#'  
#' \code{tidy_chisq} is deprecated. Use \code{\link[broom]{augment.htest}} 
#' from \pkg{broom} instead.
#'  
#' \code{ggchisq_res} is now deprecated. You could use instead \code{\link[GGally]{ggtable}} 
#' from \pkg{GGally} or \code{\link{ggcross}}.
#'
#' @name deprecated
#' @param ... parameters
NULL

#' @rdname deprecated
#' @export
ggchisq_res <- function(...) {
  stop("This function is deprecated. You could use instead JLutils::ggcross() or GGally::ggtable.")
}
