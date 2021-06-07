#' Extend geom_smooth in a single direction
#' 
#' Allows to extend a linear regression line drawn with [ggplot2::geom_smooth()]
#' to only one direction (left or right), or to zero.  
#' **DOES NOT WORK WITH CURRENT VERSION OF GGPLOT2.**
#' You have to include
#' `source("https://raw.githubusercontent.com/larmarange/JLutils/master/R/lm_right.R")`
#' into your R code.
#' 
#' cf. https://github.com/tidyverse/ggplot2/issues/3132
#' The implementation should be through a new stat rather than extending 
#' `predictdf()`
#' 
#' Note: xseq could be passed to [ggplot2::geom_smooth()]
#' 
#' @note Adapted from <https://stackoverflow.com/questions/26705554/extend-geom-smooth-in-a-single-direction>
#' @param formula a model formula
#' @param data a data.frame
#' @param ... unused
#' @export
#' @examples 
#' p <- ggplot(iris) + 
#'   aes(x = Petal.Width, y = Petal.Length, colour = Species) +
#'   geom_point()
#'   
#' # classic geom_smooth
#' p + geom_smooth(method = "lm")
#' 
#' # use `fullrange = TRUE` to extend on both sides
#' p + geom_smooth(method = "lm", fullrange = TRUE)
#' 
#' p + geom_smooth(method = "lm", xseq = c(.75, 2.25))
#' 
#' # use `method = "lm_right"` or `method = "lm_left"`
#' # with `fullrange = TRUE` to extand in only one direction
#' p + geom_smooth(method = "lm_right", fullrange = TRUE)
#' p + geom_smooth(method = "lm_left", fullrange = TRUE)
#' 
#' # use `method = "lm_zero"` to include zero
#' p + geom_smooth(method = "lm_zero")
lm_right <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_right", class(mod))
  mod
}

#' @export
#' @rdname lm_right
lm_left <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_left", class(mod))
  mod
}

#' @export
#' @rdname lm_right
lm_zero <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_zero", class(mod))
  mod
}

#' @export
predictdf.lm_right <-
  function(model, xseq, se, level) {
    ## here the main code: truncate to x values at the right
    init_range <- range(model$model$x)
    xseq <- xseq[xseq >= init_range[1]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }

#' @export
predictdf.lm_left <-
  function(model, xseq, se, level) {
    init_range <- range(model$model$x)
    ## here the main code: truncate to x values at the left
    xseq <- xseq[xseq <= init_range[2]]
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }

#' @export
predictdf.lm_zero <-
  function(model, xseq, se, level) {
    ## here the main code: take into account 0
    init_range <- range(0, model$model$x, xseq)
    xseq <- seq(init_range[1], init_range[2], length.out = 25)
    ggplot2:::predictdf.default(model, xseq[-length(xseq)], se, level)
  }
