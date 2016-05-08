#' @export
StatFillLabels <- ggproto("StatFillLabels", StatCount,
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
                       plyr::ddply(res, "x", plyr::mutate, prop = count/sum(count), y = (cumsum(count) - count / 2)/sum(count), na.rm = TRUE)
                     },
                     default_aes = aes(y = ..y.., label = paste0(round(100 * ..prop.., digits =1), "%")) 
)

#' Computes labels for position = "fill"
#' 
#' This stat makes it easier to display labels with \code{position_fill}.
#' 
#' @details 
#' Computed variables: \code{count}, the number of observations; \code{prop}, proportion on \code{x}; \code{y}, \code{y} position of labels.
#' 
#' @examples 
#' ggplot(as.data.frame(Titanic)) + aes(x = Class, fill = Survived, weight = Freq) + geom_bar(position = "fill") + geom_text(stat = "fill_labels")
#' ggplot(as.data.frame(Titanic)) + aes(x = as.integer(Class), fill = Survived, weight = Freq) + geom_area(position = "fill", stat = "count") + geom_text(stat = "fill_labels")
#' @export
stat_fill_labels <- function(mapping = NULL, data = NULL, geom = "bar",
                       position = "stack", width = NULL, na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatFillLabels, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
