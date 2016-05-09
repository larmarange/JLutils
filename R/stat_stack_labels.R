#' @export
StatStackLabels <- ggproto("StatStackLabels", StatCount,
                     compute_panel = function (self, data, scales, ...) 
                     {
                       if (ggplot2:::empty(data)) 
                         return(data.frame())
                       groups <- split(data, data$group)
                       stats <- lapply(groups, function(group) {
                         self$compute_group(data = group, scales = scales, ...)
                       })
                       stats <- mapply(function(new, old) {
                         if (ggplot2:::empty(new)) 
                           return(data.frame())
                         unique <- ggplot2:::uniquecols(old)
                         missing <- !(names(unique) %in% names(new))
                         cbind(new, unique[rep(1, nrow(new)), missing, drop = FALSE])
                       }, stats, groups, SIMPLIFY = FALSE)
                       res <- do.call(plyr::rbind.fill, stats)
                       plyr::ddply(res, "x", plyr::mutate, cumcount = cumsum(count), ylabel = cumsum(count) - count / 2, na.rm = TRUE)
                     },
                     default_aes = aes(y = ..ylabel.., label = ..count..) 
)

#' Computes labels position for position = "stack"
#' 
#' This stat makes it easier to display labels with \code{position_stack}.
#' 
#' @details 
#' Computed variables: \code{count}, the number of observations; \code{cumcount}, cumulative count; \code{ylabel}, \code{y} position of labels.
#' 
#' @examples 
#' ggplot(as.data.frame(Titanic)) + 
#'   aes(x = Class, fill = Survived, weight = Freq) + 
#'   geom_bar() + geom_text(stat = "stack_labels")
#' ggplot(as.data.frame(Titanic)) + 
#'   aes(x = Class, fill = Survived, weight = Freq) + 
#'   geom_bar() + stat_stack_labels()
#' ggplot(as.data.frame(Titanic)) + 
#'   aes(x = Class, fill = Survived, weight = Freq) + 
#'   geom_bar() + stat_stack_labels() + facet_grid(~Sex)
#' ggplot(as.data.frame(Titanic)) + 
#'   aes(x = as.integer(Class), fill = Survived, weight = Freq) + 
#'   geom_area(stat = "count") + stat_stack_labels()
#' @export
stat_stack_labels <- function(mapping = NULL, data = NULL, geom = "text",
                       position = "identity", width = NULL, na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatStackLabels, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
