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
#' \code{tidy_detailed} is deprecated. Use \code{\link[broom.helpers]{tidy_plus_plus}} from \pkg{broom.helpers} instead.
#'
#' \code{ggchisq_res} and \code{\link{ggcross}} are now deprecated. You could use instead \code{\link[GGally]{ggtable}} from \pkg{GGally}.
#'
#' @name deprecated
#' @param ... parameters
NULL

#' @rdname deprecated
#' @export
ggchisq_res <- function(...) {
  stop("This function is deprecated. You could use instead JLutils::ggcross() or GGally::ggtable().")
}

#' @rdname deprecated
#' @export
stat_stack_labels <- function(...) {
  stop("stat_stack_labels() is depecreated. Use instead GGally::stat_prop().")
}

#' @rdname deprecated
#' @export
stat_fill_labels <- function(...) {
  stop("stat_fill_labels() is depecreated. Use instead GGally::stat_prop().")
}
