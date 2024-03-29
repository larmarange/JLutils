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
#' @return a formatted character vector or, for \code{label_*} functions, a function with single parameter \code{x}, a numeric vector, that
#'   returns a character vector
#' @inheritParams scales::number
#' @param x a numeric vector to format
#' @importFrom scales number
#' @importFrom scales label_number
#' @importFrom scales percent
#' @importFrom scales label_percent
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
#' per_mille <- label_number(scale = 1000, suffix = "\u2030", accuracy = .1)
#' per_mille(v)
number <- scales::number

#' @export
#' @rdname number
label_number <- scales::label_number

#' @export
#' @rdname number
label_en <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
  label_number(
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
label_fr <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  label_number(
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
label_percent <- scales::label_percent

#' @export
#' @rdname number
percent <- scales::percent

#' @export
#' @rdname number
label_pourcent <- function(accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
  label_number(
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
comp_label_percent <- function(accuracy = 1, scale = 100, prefix = "", suffix = "%", big.mark = " ", decimal.mark = ".", trim = TRUE, ...) {
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
comp_label_pourcent <- function(accuracy = 1, scale = 100, prefix = "", suffix = " %", big.mark = " ", decimal.mark = ",", trim = TRUE, ...) {
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
en0 <- label_en(accuracy = 1)

#' @rdname number
#' @export
en1 <- label_en(accuracy = .1)

#' @rdname number
#' @export
en2 <- label_en(accuracy = .01)

#' @rdname number
#' @export
en3 <- label_en(accuracy = .001)

#' @rdname number
#' @export
en4 <- label_en(accuracy = .0001)

#' @rdname number
#' @export
en5 <- label_en(accuracy = .00001)

#' @rdname number
#' @export
fr0 <- label_fr(accuracy = 1)

#' @rdname number
#' @export
fr1 <- label_fr(accuracy = .1)

#' @rdname number
#' @export
fr2 <- label_fr(accuracy = .01)

#' @rdname number
#' @export
fr3 <- label_fr(accuracy = .001)

#' @rdname number
#' @export
fr4 <- label_fr(accuracy = .0001)

#' @rdname number
#' @export
fr5 <- label_fr(accuracy = .00001)

#' @rdname number
#' @export
percent0 <- label_percent(accuracy = 1)

#' @rdname number
#' @export
percent1 <- label_percent(accuracy = .1)

#' @rdname number
#' @export
percent2 <- label_percent(accuracy = .01)

#' @rdname number
#' @export
percent3 <- label_percent(accuracy = .001)

#' @rdname number
#' @export
percent4 <- label_percent(accuracy = .0001)

#' @rdname number
#' @export
percent5 <- label_percent(accuracy = .00001)

#' @rdname number
#' @export
pourcent0 <- label_pourcent(accuracy = 1)

#' @rdname number
#' @export
pourcent1 <- label_pourcent(accuracy = .1)

#' @rdname number
#' @export
pourcent2 <- label_pourcent(accuracy = .01)

#' @rdname number
#' @export
pourcent3 <- label_pourcent(accuracy = .001)

#' @rdname number
#' @export
pourcent4 <- label_pourcent(accuracy = .0001)

#' @rdname number
#' @export
pourcent5 <- label_pourcent(accuracy = .00001)

#' @rdname number
#' @export
comp_percent0 <- comp_label_percent(accuracy = 1)

#' @rdname number
#' @export
comp_percent1 <- comp_label_percent(accuracy = .1)

#' @rdname number
#' @export
comp_percent2 <- comp_label_percent(accuracy = .01)

#' @rdname number
#' @export
comp_percent3 <- comp_label_percent(accuracy = .001)

#' @rdname number
#' @export
comp_percent4 <- comp_label_percent(accuracy = .0001)

#' @rdname number
#' @export
comp_percent5 <- comp_label_percent(accuracy = .00001)

#' @rdname number
#' @export
comp_pourcent0 <- comp_label_pourcent(accuracy = 1)

#' @rdname number
#' @export
comp_pourcent1 <- comp_label_pourcent(accuracy = .1)

#' @rdname number
#' @export
comp_pourcent2 <- comp_label_pourcent(accuracy = .01)

#' @rdname number
#' @export
comp_pourcent3 <- comp_label_pourcent(accuracy = .001)

#' @rdname number
#' @export
comp_pourcent4 <- comp_label_pourcent(accuracy = .0001)

#' @rdname number
#' @export
comp_pourcent5 <- comp_label_pourcent(accuracy = .00001)


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
#' @return `label_pvalue` returns a function with single parameter
#'   `x`, a numeric vector, that returns a character vector.
#' @inheritParams scales::pvalue
#' @importFrom scales pvalue
#' @importFrom scales label_pvalue
#' @export
#' @examples
#' p <- c(.50, 0.12, .045, .011, .009, .00002, NA)
#' pvalue(p)
#' pvalue(p, accuracy = .01)
#' pvalue(p, add_p = TRUE)
#' custom_function <- label_pvalue(accuracy = .1, decimal.mark = ",")
#' custom_function(p)
pvalue <- scales::pvalue

#' @rdname pvalue
#' @export
label_pvalue <- scales::label_pvalue

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
