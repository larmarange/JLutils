# Adapted from https://github.com/NightingaleHealth/ggforestplot/blob/master/R/geom_stripes.R
#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @param xfrom,xto limitation of the strips along the x-axis
#' @export
#' @examples
#' data(tips, package = "reshape")
#' p <- ggplot(tips) +
#'   aes(x = time, y = day) +
#'   geom_count() +
#'   theme_light()
#' p
#' p + geom_stripped_rows()
#' p + geom_stripped_cols()
#' p + geom_stripped_rows() + geom_stripped_cols()
#'
#' p <- ggplot(tips) +
#'   aes(x = total_bill, y = day) +
#'   geom_count() +
#'   theme_light()
#' p
#' p + geom_stripped_rows()
#' p + geom_stripped_rows(xfrom = 10, xto = 35)
#' p + geom_stripped_rows(odd = "blue", even = "yellow")
#' p + geom_stripped_rows(odd = "blue", even = "yellow", alpha = .1)
geom_stripped_rows <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               xfrom = -Inf,
                               xto = Inf) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStrippedRows,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xfrom = xfrom, xto = xto, ...)
  )
}

GeomStrippedRows <- ggplot2::ggproto("GeomStrippedRows", ggplot2::Geom,
  required_aes = c("y"),

  default_aes = ggplot2::aes(
    odd = "#11111111", even = "#00000000",
    alpha = NA, colour = "black", linetype = "solid", size = NA
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(data, panel_params, coord, xfrom, xto) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(.data$y),
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5,
          xmin = xfrom,
          xmax = xto
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)

#' @rdname geom_stripped_rows
#' @param yfrom,yto limitation of the strips along the y-axis
#' @export
geom_stripped_cols <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               yfrom = -Inf,
                               yto = Inf) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStrippedCols,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(yfrom = yfrom, yto = yto, ...)
  )
}

GeomStrippedCols <- ggplot2::ggproto("GeomStrippedCols", ggplot2::Geom,
  required_aes = c("x"),

  default_aes = ggplot2::aes(
    odd = "#11111111", even = "#00000000",
    alpha = NA, colour = "black", linetype = "solid", size = NA
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(data, panel_params, coord, yfrom, yto) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          x = round(.data$x),
          xmin = .data$x - 0.5,
          xmax = .data$x + 0.5,
          ymin = yfrom,
          ymax = yto
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$xmin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)
