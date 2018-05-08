precision <- function(x) {
  rng <- range(x, na.rm = TRUE)
  
  span <- if (zero_range(rng)) abs(rng[1]) else diff(rng)
  if (span == 0)
    return(1)
  
  10 ^ floor(log10(span))
}


#' Number formatters
#' 
#' \code{number} is a generic formatter for numeric values.
#' \code{en} is a shortcut for English format (comma as separator for thousands, point for decimal), \code{fr} for French format (space for thousands, comma for decimal).
#' \code{percent} a shortcut for English percentages (value are multiplied by 100 and a % symbol is added) and \code{pourcent} a shortcut for French percentages.
#' \code{comp_percent} returns the complement of 1, i.e. \code{percent(1 - x)}. \code{comp_pourcent} is the French version.
#' \code{*_format} functions will return another functions, useful to be used for example with \code{ggplot2}.
#' \code{en0} to \code{en5} are shortcuts for \code{en} with 0 to 5 digits after decimal point. Similarly, \code{fr0} to \code{fr5} are shortcuts of \code{fr},
#' \code{percent0} to \code{percent5} of \code{percent}, etc.
#'
#' @return a formatted character vector or, for \code{*_format} functions, a function with single parameter \code{x}, a numeric vector, that
#'   returns a character vector
#' @param x a numeric vector to format
#' @param accuracy number to round to, \code{NULL} for automatic guess
#' @param multiplier number to multiply by (e.g. for computing percentages or thousands)
#' @param prefix,suffix Symbols to display before and after value
#' @param big.mark character used between every 3 digits to separate thousands
#' @param decimal.mark the character to be used to indicate the numeric decimal point
#' @param ... other arguments passed on to \code{\link{format}}.
#' @rdname number
#' @export
#' @examples
#' v <- c(12.3, 4, 12345.789, 0.0002)
#' number(v)
#' en(v)
#' fr(v)
#' en2(v)
#' en(v, accuracy = .001)
#' en(v, accuracy = .5)
#' 
#' p <- runif(10)
#' p
#' percent(p)
#' percent2(p)
#' pourcent2(p)
#' comp_percent(p)
#'
#' # Per mille
#' per_mille <- number_format(multiplier = 1000, suffix = "\u2030", accuracy = .1)
#' per_mille(v)
number_format <- function(accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ".", ...) {
  function(x) number(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}
#' @export
#' @rdname number
number <- function(x, accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ".", ...) {
  if (length(x) == 0) return(character())
  if (is.null(accuracy)) {
    x <- round_any(x, precision(x) / multiplier)
    nsmall <- -floor(log10(precision(x)))
  } else {
    x <- round_any(x, accuracy / multiplier)
    nsmall <- -floor(log10(accuracy))
  }
  nsmall <- min(max(nsmall, 0), 20)
  paste0(prefix, format(multiplier * x, big.mark = big.mark, decimal.mark = decimal.mark, scientific = FALSE, trim = TRUE, nsmall = nsmall, ...), suffix)
}

#' @export
#' @rdname number
en_format <- function(accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = ",", decimal.mark = ".", ...) {
  number_format(accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}
#' @export
#' @rdname number
en <- function(x, accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = ",", decimal.mark = ".", ...) {
  number(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}

#' @export
#' @rdname number
fr_format <- function(accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ",", ...) {
  number_format(accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}
#' @export
#' @rdname number
fr <- function(x, accuracy = 1, multiplier = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ",", ...) {
  number(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}

#' @export
#' @rdname number
percent_format <- function(accuracy = 1, multiplier = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", ...) {
  number_format(accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}
#' @export
#' @rdname number
percent <- function(x, accuracy = 1, multiplier = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", ...) {
  number(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}

#' @export
#' @rdname number
pourcent_format <- function(accuracy = 1, multiplier = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", ...) {
  number_format(accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}
#' @export
#' @rdname number
pourcent <- function(x, accuracy = 1, multiplier = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", ...) {
  number(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}

#' @export
#' @rdname number
comp_percent_format <- function(accuracy = 1, multiplier = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", ...) {
  function(x) {
    comp_percent(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
  }
}

#' @export
#' @rdname number
comp_percent <- function(x, accuracy = 1, multiplier = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", ...) {
  number(1 - x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}

#' @export
#' @rdname number
comp_pourcent_format <- function(accuracy = 1, multiplier = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", ...) {
  function(x) {
    comp_pourcent(x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
  }
}

#' @export
#' @rdname number
comp_pourcent <- function(x, accuracy = 1, multiplier = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", ...) {
  number(1 - x, accuracy, multiplier, prefix, suffix, big.mark, decimal.mark, ...)
}


#' @rdname number
#' @export
en0 <- en_format(accuracy = 1)

#' @rdname number
#' @export
en1 <- en_format(accuracy = .1)

#' @rdname number
#' @export
en2 <- en_format(accuracy = .01)

#' @rdname number
#' @export
en3 <- en_format(accuracy = .001)

#' @rdname number
#' @export
en4 <- en_format(accuracy = .0001)

#' @rdname number
#' @export
en5 <- en_format(accuracy = .00001)

#' @rdname number
#' @export
fr0 <- fr_format(accuracy = 1)

#' @rdname number
#' @export
fr1 <- fr_format(accuracy = .1)

#' @rdname number
#' @export
fr2 <- fr_format(accuracy = .01)

#' @rdname number
#' @export
fr3 <- fr_format(accuracy = .001)

#' @rdname number
#' @export
fr4 <- fr_format(accuracy = .0001)

#' @rdname number
#' @export
fr5 <- fr_format(accuracy = .00001)

#' @rdname number
#' @export
percent0 <- percent_format(accuracy = 1)

#' @rdname number
#' @export
percent1 <- percent_format(accuracy = .1)

#' @rdname number
#' @export
percent2 <- percent_format(accuracy = .01)

#' @rdname number
#' @export
percent3 <- percent_format(accuracy = .001)

#' @rdname number
#' @export
percent4 <- percent_format(accuracy = .0001)

#' @rdname number
#' @export
percent5 <- percent_format(accuracy = .00001)

#' @rdname number
#' @export
pourcent0 <- pourcent_format(accuracy = 1)

#' @rdname number
#' @export
pourcent1 <- pourcent_format(accuracy = .1)

#' @rdname number
#' @export
pourcent2 <- pourcent_format(accuracy = .01)

#' @rdname number
#' @export
pourcent3 <- pourcent_format(accuracy = .001)

#' @rdname number
#' @export
pourcent4 <- pourcent_format(accuracy = .0001)

#' @rdname number
#' @export
pourcent5 <- pourcent_format(accuracy = .00001)

#' @rdname number
#' @export
comp_percent0 <- comp_percent_format(accuracy = 1)

#' @rdname number
#' @export
comp_percent1 <- comp_percent_format(accuracy = .1)

#' @rdname number
#' @export
comp_percent2 <- comp_percent_format(accuracy = .01)

#' @rdname number
#' @export
comp_percent3 <- comp_percent_format(accuracy = .001)

#' @rdname number
#' @export
comp_percent4 <- comp_percent_format(accuracy = .0001)

#' @rdname number
#' @export
comp_percent5 <- comp_percent_format(accuracy = .00001)

#' @rdname number
#' @export
comp_pourcent0 <- comp_pourcent_format(accuracy = 1)

#' @rdname number
#' @export
comp_pourcent1 <- comp_pourcent_format(accuracy = .1)

#' @rdname number
#' @export
comp_pourcent2 <- comp_pourcent_format(accuracy = .01)

#' @rdname number
#' @export
comp_pourcent3 <- comp_pourcent_format(accuracy = .001)

#' @rdname number
#' @export
comp_pourcent4 <- comp_pourcent_format(accuracy = .0001)

#' @rdname number
#' @export
comp_pourcent5 <- comp_pourcent_format(accuracy = .00001)


#' Formatting numbers like in WHO publications
#' 
#' In WHO publications, population numbers are usually presented as follow:
#' \itemize{
#'   \item 0 if < 5
#'   \item 10 if ≥ 5 and < 10
#'   \item one significant digit if ≥ 10 and < 100
#'   \item two significant digits if ≥ 100 and < 10 millions
#'   \item three significant digits if ≥ 10 millions
#' }
#' 
#' @param x a numeric vector to format
#' 
#' @export
#' @examples 
#' who_formatter(c(3, 8, 42, 75, 45678, 9876543, 12345678))
who_formatter <- function(x) {
  x[x < 4 & !is.na(x)] <- 0
  x[x >= 5 & x < 10 & !is.na(x)] <- 10
  x[x >= 10 & x < 100 & !is.na(x)] <- signif(x[x >= 10 & x < 100 & !is.na(x)], digits=1)
  x[x >= 100 & x < 10000000 & !is.na(x)] <- signif(x[x >= 100 & x < 10000000 & !is.na(x)], digits=2)
  x[x >= 10000000 & !is.na(x)] <- signif(x[x >= 10000000 & !is.na(x)], digits=3)
  
  return(format(x, digits=0, big.mark=" ", scientific=FALSE))
}


#' p-values formatter and significance stars
#' 
#' Formatter for p-values, adding a symbol "<" for very small p-values and, optionally, significance stars
#' 
#' @param x a numeric vector of p-values
#' @param formatter a formatter function, see \code{\link{number}}, typically \code{\link{en}} or \code{\link{fr}}
#' @param accuracy number to round to
#' @param stars add significance stars?
#' @param three level below which to display three stars '***'
#' @param two level below which to display two stars '**'
#' @param one level below which to display one star '*'
#' @param point level below which to display a point '.' (\code{NULL} to not display a point)
#' @param ... additional parameters sent to formatter function
#' @details 
#' \code{pval_format} will produce a custom function, to be used for example with \code{ggplot2}.
#' @export
#' @examples 
#' p <- c(.50, 0.12, .09, .045, .011, .009, .00002, NA)
#' pval(p)
#' pval(p, formatter = fr, accuracy = .01)
#' pval(p, stars = TRUE)
pval <- function(x, formatter = en, accuracy = .001, stars = FALSE, three = 0.001, two = 0.01, one = 0.05, point = 0.1, ...) {
  res <- formatter(x, accuracy = accuracy, ...)
  digits <- -floor(log10(accuracy))
  res[x < 10 ^ -digits] <- paste0("<", formatter(10 ^ -digits, accuracy = accuracy, ...))
  if (stars)
    res <- paste(res, signif_stars(x, three, two, one, point))
  res
}

#' @rdname pval
#' @export
#' @examples 
#' custom_function <- pval_format(accuracy = .1, stars = TRUE)
#' custom_function(p)
pval_format <- function(formatter = en, accuracy = .001, stars = FALSE, three = 0.001, two = 0.01, one = 0.05, point = 0.1) {
  function(x) {
    pval(x, formatter, accuracy, stars, three, two, one, point)
  }
}

#' @rdname pval
#' @export signif_stars
#' @examples 
#' signif_stars(p)
#' signif_stars(p, one = .15, point = NULL)
signif_stars <- function(x, three = 0.001, two = 0.01, one = 0.05, point = 0.1) {
  res <- rep_len("", length.out = length(x))
  if (!is.null(point))
    res[x <= point] <- "."
  if (!is.null(one))
    res[x <= one] <- "*"
  if (!is.null(two))
    res[x <= two] <- "**"
  if (!is.null(three))
    res[x <= three] <- "***"
  res
}



