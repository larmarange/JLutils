#' Compute weighted y mean
#' 
#' Experimental
#'
#' @inheritParams ggplot2::stat_bin
#' @export
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

