#' Compute cross-tabulation statisticics
#' 
#' `stat_cross()` computes statistics of a 2-dimensional matrix using `broom::augment.htest()`.
#' 
#' @inheritParams ggplot2::stat_identity
#' @param geom Override the default connection between `geom_bar()` and `stat_prop()`.
#' @param na.rm If `TRUE`, the default, missing values are removed with a warning. 
#'   If `TRUE`, missing values are silently removed.
#' @section Aesthetics:
#' `stat_prop()` requires the \strong{x} and the \strong{y} aesthetics.
#' @section Computed variables:
#' \describe{
#'   \item{observed}{number of observations in x,y}
#'   \item{prop}{proportion of total}
#'   \item{row.prop}{row proportion}
#'   \item{col.prop}{column proportion}
#'   \item{expected}{expected count under the null hypothesis}
#'   \item{residuals}{Pearson's residual}
#'   \item{stdres}{standardized residual}
#' }
#'
#' @import ggplot2
#' @importFrom broom augment
#' @export
#' @examples 
#' d <- as.data.frame(Titanic)
#' 
#' ggplot(d) + 
#'   aes(x = Class, y = Survived, weight = Freq) + 
#'   stat_cross() + 
#'   scale_size_area()
#' 
#' ggplot(d) + 
#'   aes(x = Class, y = Survived, weight = Freq, fill = ..stdres..) + 
#'   stat_cross(shape = 22) + 
#'   scale_fill_steps2(breaks = c(-4, -2, 2, 4), show.limits = TRUE)
#' 
#' ggplot(d) + 
#'   aes(x = Class, y = Survived, weight = Freq, label = scales::percent(..row.prop..), size = NULL) +
#'   geom_text(stat = "cross")
#'   
#'   ggplot(d) + 
#'     aes(
#'       x = Class, y = Survived, weight = Freq, 
#'       label = scales::percent(..row.prop..), 
#'       size = NULL, fill = ..stdres..
#'     ) + 
#'     stat_cross(shape = 22, size = 30) + 
#'     geom_text(stat = "cross") + 
#'     scale_fill_steps2(breaks = c(-4, -2, 2, 4), show.limits = TRUE) + 
#'     facet_grid(Sex ~ .) +
#'     labs(fill = "Standardized residuals") +
#'     theme_minimal()
#' 
stat_cross <- function(mapping = NULL, data = NULL,
                       geom = "point", position = "identity",
                       ...,
                       na.rm = TRUE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  
  params <- list(
    na.rm = na.rm,
    ...
  )
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatCross,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname stat_cross
#' @format NULL
#' @usage NULL
#' @export
StatCross <- ggproto("StatCross", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(
    size = ..observed.., 
    weight = 1
  ),
  
  setup_params = function(data, params) {
    params
  },
  
  extra_params = c("na.rm"),
                    
  compute_panel = function(self, data, scales) {
   data$weight <- data$weight %||% rep(1, nrow(data))

   # compute cross statistics
   panel <- broom::augment(chisq.test(xtabs(weight ~ y + x, data = data)))

   names(panel)[which(names(panel) == ".observed")] <- "observed"
   names(panel)[which(names(panel) == ".prop")] <- "prop"
   names(panel)[which(names(panel) == ".row.prop")] <- "row.prop"
   names(panel)[which(names(panel) == ".col.prop")] <- "col.prop"
   names(panel)[which(names(panel) == ".expected")] <- "expected"
   names(panel)[which(names(panel) == ".residuals")] <- "residuals"
   names(panel)[which(names(panel) == ".stdres")] <- "stdres"

   data <- merge(data, panel, by = c("x", "y"), all.x = TRUE)
   data
  }
)


