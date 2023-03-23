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
#' `signif_stars()` is now available in `ggstats`, see `ggstats::signif_stars()`.
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
    "ggstats::stat_prop()"
  )
}

#' @rdname deprecated
#' @export
stat_fill_labels <- function(...) {
  lifecycle::deprecate_stop(
    "1.21.0",
    "JLutils::stat_fill_labels()",
    "ggstats::stat_prop()"
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

#' @rdname deprecated
#' @export
signif_stars <- function(...) {
  lifecycle::deprecate_stop(
    "1.24.0",
    "JLutils::signif_stars()",
    "ggstats::signif_stars()"
  )
}
