#' Compute weighted y mean
#' 
#' Experimental
#'
#' @inheritParams ggplot2::stat_bin
#' @export
#' @examples
#' cuse <- read.table("https://data.princeton.edu/wws509/datasets/cuse.dat", header = TRUE)
#' cuse$n <- cuse$notUsing + cuse$using
#' cuse$prop <- cuse$using / cuse$n
#' 
#' library(ggplot2)
#' ggplot(cuse) +
#'   aes(x = education, y = prop, weight = n) +
#'   stat_weighted_mean()
#'   
#' ggplot(cuse) +
#'   aes(x = age, y = prop, weight = n, color = education) +
#'   stat_weighted_mean()
#'   
#' ggplot(cuse) +
#'   aes(x = education, y = prop, weight = n) +
#'   stat_weighted_mean(geom = "bar")
#'
#' ggplot(cuse) +
#'   aes(x = age, y = prop, weight = n, fill = education) +
#'   stat_weighted_mean(geom = "bar") +
#'   geom_text(aes(label = scales::percent(after_stat(y))), stat = "weighted_mean", vjust = 0) +
#'   facet_grid(~ education)
stat_weighted_mean <- function(mapping = NULL, data = NULL,
                         geom = "point", position = "identity",
                         ...,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatWeightedMean,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname stat_weighted_mean
#' @format NULL
#' @usage NULL
#' @export
StatWeightedMean <- ggproto("StatSummary", Stat,
                       required_aes = c("x", "y"),
                       extra_params = c("na.rm", "orientation"),
                       setup_params = function(data, params) {
                         params$flipped_aes <- has_flipped_aes(data, params)
                         params
                       },
                       
                       compute_panel = function(data, scales, na.rm = FALSE, flipped_aes = FALSE) {
                         data <- ggplot2::flip_data(data, flipped_aes)
                         if (is.null(data$weight))
                           data$weight <- rep(1, nrow(data))
                         
                         summarised <- aggregate(
                           cbind(numerator = y * weight, denominator = weight) ~ ., 
                           data, FUN = sum, na.rm = TRUE
                         )
                         summarised$y <- summarised$numerator / summarised$denominator
                         
                         summarised$flipped_aes <- flipped_aes
                         ggplot2::flip_data(summarised, flipped_aes)
                       }
)

