#' Compute proportions according to custom denominator
#' 
#' `stat_prop()` is a variation of `stat_count()` allowing to compute custom 
#' proportions according to the `prop_among` aesthetic defining the denominator
#' (i.e. all proportions for a same value of `prop_among` will sum to 1)
#' 
#' @inheritParams ggplot2::stat_count
#' @param geom Override the default connection between `geom_bar()` and `stat_prop()`.
#' @section Aesthetics:
#' `stat_prop()` requires the \strong{prop_among} aesthetic.
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
#' @examples 
#' 
#' d <- as.data.frame(Titanic)
#' 
#' p <- ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_among = Class) +
#'   geom_bar(position = "fill") + 
#'   geom_text(stat = "prop", position = position_fill(.5))
#' p
#' p + facet_grid(~ Sex)
#' 
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_among = Survived) +
#'   geom_bar(position = "dodge") +
#'   geom_text(stat = "prop", position = position_dodge(0.9), vjust = "bottom")
#'   
#' ggplot(d) +
#'   aes(x = Class, fill = Survived, weight = Freq, prop_among = 1) +
#'   geom_bar() +
#'   geom_text(aes(label = scales::percent(..prop.., accuracy = .1)), stat = "prop", position = position_stack(.5))
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
    stop("stat_prop() must not be used with a y aesthetic.", call. = FALSE)
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
  required_aes = c("x", "prop_among"),
  default_aes = aes(
    y = stat(count), weight = 1,
    label = scales::percent(..prop..)
  ),
  
  setup_params = function(data, params) {
    if (!is.null(data$y)) {
      stop("stat_prop() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },       
                    
  compute_panel = function(self, data, scales, width = NULL) {
   data$weight <- data$weight %||% rep(1, nrow(data))
   width <- width %||% (ggplot2::resolution(data$x) * 0.9)
   
   # sum weights for each combination of prop_among and aesthetics
   # the use of . allows to consider all aesthetics defined in data
   panel <- aggregate(weight ~ ., data = data, sum, na.rm = TRUE)
   names(panel)[which(names(panel) == "weight")] <- "count"
   panel$count[is.na(panel$count)] <- 0
   
   # compute proportions by prop_among
   f <- function(x) {sum(abs(x))}
   panel$prop <- panel$count / ave(panel$count, panel$prop_among, FUN = f)
   panel$width <- width
  
   panel
  }
)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

