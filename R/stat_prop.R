#' Compute proportions according to custom denominator
#' 
#' `stat_prop()` is a variation of `stat_count()` allowing to compute custom 
#' proportions according to the `prop_of` aesthetic defining the denominator
#' (i.e. all proportions for a same value of `prop_of` will sum to 1)
#' 
#' @inheritParams ggplot2::stat_count
#' @param geom Override the default connection between `geom_bar()` and `stat_prop()`.
#' @section Aesthetics:
#' `stat_prop()` requires the \strong{prop_of} aesthetic.
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{computed proportion}
#' }
#' @seealso \code{\link[ggplot2]{stat_count}}
#'
#' @import ggplot2
#' @importFrom scales percent
#' @export
#' 
#' d <- as.data.frame(Titanic)
#' 
#' p <- ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_of = Class) +
#'   geom_bar(position = "fill") + 
#'   geom_text(stat = "prop", position = position_fill(.5))
#' p
#' p + facet_grid(~ Sexe)
#' 
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_of = Survived) +
#'   geom_bar(position = "dodge") +
#'   geom_text(stat = "prop", position = position_dodge(0.9), vjust = "bottom")
#'   
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_of = 1) +
#'   geom_bar() +
#'   geom_text(aes(label = scales::percent(after_stat(prop), accuracy = .1)), stat = "prop", position = position_stack(.5))
#' 
stat_prop <- function(mapping = NULL, data = NULL,
                       geom = "bar", position = "fill",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       orientation = NA,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  
  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    abort("stat_prop() must not be used with a y aesthetic.")
  }
  if (!is.null(params$prop_of)) {
    abort("stat_prop() must not be used with a prop_of aesthetic.")
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatProp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_prop
#' @format NULL
#' @usage NULL
#' @export
StatProp <- ggproto("StatProp", Stat,
                     required_aes = c("x|y", "prop_of"),
                     
                     default_aes = aes(
                       x = after_stat(count), y = after_stat(count), 
                       label = scales::percent(after_stat(prop)), 
                       weight = 1
                      ),
                     
                     setup_params = function(data, params) {
                       params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
                       
                       has_x <- !(is.null(data$x) && is.null(params$x))
                       has_y <- !(is.null(data$y) && is.null(params$y))
                       if (!has_x && !has_y) {
                         abort("stat_prop() requires an x or y aesthetic.")
                       }
                       if (has_x && has_y) {
                         abort("stat_prop() can only have an x or y aesthetic.")
                       }
                       
                       params
                     },
                     
                     extra_params = c("na.rm", "orientation"),
                     
                     compute_panel = function(self, data, scales, width = NULL, flipped_aes = FALSE) {
                       data <- ggplot2::flip_data(data, flipped_aes)
                       data$weight <- data$weight %||% rep(1, nrow(data))
                       width <- width %||% (ggplot2::resolution(data$x) * 0.9)
                       
                       # sum weights for each combination of prop_of and aesthetics
                       # the use of . allows to consider all aesthetics defined in data
                       panel <- aggregate(weight ~ ., data = data, sum, na.rm = TRUE)
                       names(panel)[which(names(panel) == "weight")] <- "count"
                       panel$count[is.na(panel$count)] <- 0
                       
                       # compute proportions by prop_of
                       f <- function(x) {sum(abs(x))}
                       panel$prop <- panel$count / ave(panel$count, panel$prop_of, FUN = f)
                       panel$width <- width

                       flip_data(panel, flipped_aes)
                     }
)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

