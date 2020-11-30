#' Deprecated functions
#'
#' @description 
#' \lifecycle{deprecated}
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
  lifecycle::deprecate_stop(
    "1.21.0", 
    "JLutils::ggchisq_res()", 
    "GGally::ggtable()"
  )
}

#' @rdname deprecated
#' @export
ggcross <- function(...) {
  lifecycle::deprecate_stop(
    "1.22.0", 
    "JLutils::ggcross()", 
    "GGally::ggtable()"
  )
}

#' @rdname deprecated
#' @export
stat_stack_labels <- function(...) {
  lifecycle::deprecate_stop(
    "1.21.0", 
    "JLutils::stat_stack_labels()", 
    "GGally::stat_prop()"
  )
}

#' @rdname deprecated
#' @export
stat_fill_labels <- function(...) {
  lifecycle::deprecate_stop(
    "1.21.0", 
    "JLutils::stat_fill_labels()", 
    "GGally::stat_prop()"
  )
}

#' @rdname deprecated
#' @export
tidy_detailed <- function(...) {
  lifecycle::deprecate_stop(
    "1.21.0", 
    "JLutils::tidy_detailed()", 
    "broom.helpers::tidy_plus_plus()"
  )
}
