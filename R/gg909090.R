#' plot the 90-90-90 UNAIDS's target
#'
#' @param first value of the first 90
#' @param second value of the second 90
#' @param third value of the third 90
#' @param col colour of the graph
#' @param sub value of the sub-labels (\code{NULL} to hide)
#' @param p_size text size of the percentages
#' @param sub_size text size of the sub-labels
#' @param overall display the overall percentage?
#' @examples
#' gg909090()
#' gg909090(0.782334, .6789, .82, col = "darkblue", overall = TRUE)
#' @export
gg909090 <- function(
                     first = 0.9, second = 0.9, third = 0.9, col = "#E81634",
                     sub = c("diagnosed", "on treatment", "virally suppressed"),
                     p_size = 6, sub_size = 4,
                     overall = FALSE) {
  if (!is.null(sub) & length(sub) != 3) {
    stop("sub should contain 3 elements")
  }

  res <- data.frame()
  x1 <- -1.2
  x2 <- 0
  x3 <- 1.2
  x4 <- 2.2
  l <- 1
  res <- rbind(res, data.frame(
    x = x1, y = 1, xmin = x1 - l / 2, xmax = x1 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "dotted", p = NA
  ))
  l <- sqrt(first)
  res <- rbind(res, data.frame(
    x = x1, y = 1, xmin = x1 - l / 2, xmax = x1 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "plain", p = first
  ))
  res <- rbind(res, data.frame(
    x = x2, y = 1, xmin = x2 - l / 2, xmax = x2 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "dotted", p = NA
  ))
  l <- sqrt(first * second)
  res <- rbind(res, data.frame(
    x = x2, y = 1, xmin = x2 - l / 2, xmax = x2 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "plain", p = second
  ))
  res <- rbind(res, data.frame(
    x = x3, y = 1, xmin = x3 - l / 2, xmax = x3 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "dotted", p = NA
  ))
  l <- sqrt(first * second * third)
  res <- rbind(res, data.frame(
    x = x3, y = 1, xmin = x3 - l / 2, xmax = x3 + l / 2,
    ymin = 1 - l / 2, ymax = 1 + l / 2, ltype = "plain", p = third
  ))

  p <- ggplot(res[res$ltype == "plain", ]) +
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
    geom_rect(data = res[res$ltype == "dotted", ], linetype = "dotted", colour = col, fill = "transparent") +
    geom_rect(fill = col) +
    coord_equal() +
    geom_text(aes(label = JLutils::percent1(p), x = x, y = y), size = p_size, colour = "white") +
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  if (!is.null(sub)) {
    p <- p +
      annotate("text", x = x1, y = 1 - 0.65, label = sub[1], size = sub_size, color = col, fontface = "bold") +
      annotate("text", x = x2, y = 1 - 0.65, label = sub[2], size = sub_size, color = col, fontface = "bold") +
      annotate("text", x = x3, y = 1 - 0.65, label = sub[3], size = sub_size, color = col, fontface = "bold")
  }

  if (overall) {
    p <- p +
      annotate("text",
        x = x4, y = 1,
        label = paste("=", percent1(first * second * third)),
        size = p_size, color = col, fontface = "bold"
      ) +
      xlim(-1.75, 2.75)
  } else {
    p <- p + xlim(-1.75, 1.75)
  }
  p
}
