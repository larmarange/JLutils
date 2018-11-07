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
#' @inheritParams scales::number
#' @importFrom scales number
#' @importFrom scales number_format
#' @importFrom scales percent
#' @importFrom scales percent_format
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
#' per_mille <- number_format(scale = 1000, suffix = "\u2030", accuracy = .1)
#' per_mille(v)
number <- scales::number

#' @export
#' @rdname number
number_format <- scales::number_format

#' @export
#' @rdname number
en_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
  number_format(
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}
#' @export
#' @rdname number
en <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
  number(x,
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}

#' @export
#' @rdname number
fr_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  number_format(
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}
#' @export
#' @rdname number
fr <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  number(x,
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}

#' @export
#' @rdname number
percent_format <- scales::percent_format

#' @export
#' @rdname number
percent <- scales::percent

#' @export
#' @rdname number
pourcent_format <- function(accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  number_format(
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}
#' @export
#' @rdname number
pourcent <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  number(x,
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}

#' @export
#' @rdname number
comp_percent_format <- function(accuracy = 1, scale = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", trim = TRUE, ...) {
  function(x) {
    comp_percent(
      accuracy = accuracy, scale = scale, 
      prefix = prefix, suffix = suffix, 
      big.mark = big.mark, decimal.mark = decimal.mark, 
      trim = trim, ...
    )
  }
}

#' @export
#' @rdname number
comp_percent <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", trim = TRUE, ...) {
  number(1 - x,
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
}

#' @export
#' @rdname number
comp_pourcent_format <- function(accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  function(x) {
    comp_pourcent(
      accuracy = accuracy, scale = scale, 
      prefix = prefix, suffix = suffix, 
      big.mark = big.mark, decimal.mark = decimal.mark, 
      trim = trim, ...
    )
  }
}

#' @export
#' @rdname number
comp_pourcent <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  number(1 - x,
    accuracy = accuracy, scale = scale, 
    prefix = prefix, suffix = suffix, 
    big.mark = big.mark, decimal.mark = decimal.mark, 
    trim = trim, ...
  )
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
#'   \item 10 if = 5 and < 10
#'   \item one significant digit if = 10 and < 100
#'   \item two significant digits if = 100 and < 10 millions
#'   \item three significant digits if = 10 millions
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
  x[x >= 10 & x < 100 & !is.na(x)] <- signif(x[x >= 10 & x < 100 & !is.na(x)], digits = 1)
  x[x >= 100 & x < 10000000 & !is.na(x)] <- signif(x[x >= 100 & x < 10000000 & !is.na(x)], digits = 2)
  x[x >= 10000000 & !is.na(x)] <- signif(x[x >= 10000000 & !is.na(x)], digits = 3)

  return(format(x, digits = 0, big.mark = " ", scientific = FALSE))
}



#' p-values formatter
#'
#' Formatter for p-values, adding a symbol "<" for small p-values.
#'
#' @return `pvalue_format` returns a function with single parameter
#'   `x`, a numeric vector, that returns a character vector.
#' @inheritParams scales::pvalue
#' @importFrom scales pvalue
#' @importFrom scales pvalue_format
#' @export
#' @examples
#' p <- c(.50, 0.12, .045, .011, .009, .00002, NA)
#' pvalue(p)
#' pvalue(p, accuracy = .01)
#' pvalue(p, add_p = TRUE)
#' custom_function <- pvalue_format(accuracy = .1, decimal.mark = ",")
#' custom_function(p)
pvalue <- scales::pvalue

#' @rdname pvalue
#' @export
pvalue_format <- scales::pvalue_format

#' @rdname pvalue
#' @export signif_stars
#' @examples
#' signif_stars(p)
#' signif_stars(p, one = .15, point = NULL)
signif_stars <- function(x, three = 0.001, two = 0.01, one = 0.05, point = 0.1) {
  res <- rep_len("", length.out = length(x))
  if (!is.null(point)) {
    res[x <= point] <- "."
  }
  if (!is.null(one)) {
    res[x <= one] <- "*"
  }
  if (!is.null(two)) {
    res[x <= two] <- "**"
  }
  if (!is.null(three)) {
    res[x <= three] <- "***"
  }
  res
}

#' Add leading zeros
#'
#' @param x a numeric vector
#' @param left_digits number of digits before decimal point, automatically computed if not provided
#' @param digits number of digits after decimal point
#' @param prefix,suffix Symbols to display before and after value
#' @param ... additional paramaters passed to \code{\link[base]{formatC}}, as \code{big.mark} or \code{decimal.mark}
#' @export
#' @seealso \code{\link[base]{formatC}}, \code{\link[base]{sprintf}}
#' @examples
#' v <- c(2, 103.24, 1042.147, 12.4566, NA)
#' leading_zeros(v)
#' leading_zeros(v, digits = 1)
#' leading_zeros(v, left_digits = 6, big.mark = " ")
#' leading_zeros(c(0, 6, 12, 18), prefix = "M")
leading_zeros <- function(x, left_digits = NULL, digits = 0, prefix = "", suffix = "", ...) {
  if (is.null(left_digits)) {
    left_digits <- trunc(max(log10(x), na.rm = TRUE)) + 1
  }
  if (digits > 0) {
    width <- left_digits + digits + 1
  } else {
    width <- left_digits
  }
  paste0(
    prefix,
    formatC(x, width = width, digits = digits, flag = "0", format = "f", preserve.width = "common", ...),
    suffix
  )
}
