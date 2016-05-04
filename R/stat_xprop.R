#' @export
StatXprop <- ggproto("StatXprop", StatCount,
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
                       plyr::ddply(res, "x", plyr::mutate, xprop = count/sum(count), na.rm = TRUE)
                     },
                     default_aes = aes(y = ..xprop..)
)

#' Computes proportions by x
#' 
#' This stat makes it easier to generate cumulative stacked bar / area charts.
#' 
#' @examples 
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(stat = "count")
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(stat = "xprop")
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(stat = "count") + facet_wrap(~Sex)
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(stat = "xprop") + facet_wrap(~Sex)
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(stat = "xprop", position = "dodge")
#' ggplot(as.data.frame(Titanic)) + aes(x = as.integer(Class), fill = Survived, weight = Freq) + geom_area(stat = "xprop")
#' @export
stat_xprop <- function(mapping = NULL, data = NULL, geom = "bar",
                       position = "stack", width = NULL, na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatXprop, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
