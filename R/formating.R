#' Percent formatter
#' 
#' Multiply by 100 and display a percent sign.
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{percent0(x)} is a shortcut for \code{percent(x, digits = 0)}, 
#' \code{percent1(x)} for \code{percent(x, digits = 1)}, etc.
#' @export percent
#' @examples
#' percent(0.004)
#' percent0(c(0.000045, 0.56789, 234.567))
#' percent2(c(0.000045, 0.56789, 234.567))
#' percent5(c(0.000045, 0.56789, 234.567))

percent <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(paste0(format(round(100*x, digits = digits), nsmall = digits, big.mark = " ", scientific = FALSE),"%"))
}

#' @rdname percent
#' @export
percent0 <- function (x)
{
  return(percent(x, digits = 0))
}

#' @rdname percent
#' @export
percent1 <- function (x)
{
  return(percent(x, digits = 1))
}

#' @rdname percent
#' @export
percent2 <- function (x)
{
  return(percent(x, digits = 2))
}

#' @rdname percent
#' @export
percent3 <- function (x)
{
  return(percent(x, digits = 3))
}

#' @rdname percent
#' @export
percent4 <- function (x)
{
  return(percent(x, digits = 5))
}

#' @rdname percent
#' @export
percent5 <- function (x)
{
  return(percent(x, digits = 5))
}

#' @rdname percent
#' @details \code{comp.percent(x)} returns the complement to 1 of \code{x}, 
#' i.e. \code{percent(1 - x)}.
#' @examples 
#' comp.percent(0.255)
#' @export
comp.percent <- function (x, digits=1) 
{
  return(percent(1 - x, digits))
}

#' @rdname percent
#' @export
comp.percent0 <- function (x)
{
  return(comp.percent(x, digits = 0))
}

#' @rdname percent
#' @export
comp.percent1 <- function (x)
{
  return(comp.percent(x, digits = 1))
}

#' @rdname percent
#' @export
comp.percent2 <- function (x)
{
  return(comp.percent(x, digits = 2))
}

#' @rdname percent
#' @export
comp.percent3 <- function (x)
{
  return(comp.percent(x, digits = 3))
}

#' @rdname percent
#' @export
comp.percent4 <- function (x)
{
  return(comp.percent(x, digits = 5))
}

#' @rdname percent
#' @export
comp.percent5 <- function (x)
{
  return(comp.percent(x, digits = 5))
}


#' Percent formatter (French)
#' 
#' Multiply by 100, use a comma as decimal separator and display a percent sign (French way).
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{pourcent0(x)} is a shortcut for \code{pourcent(x, digits = 0)}, 
#' \code{pourcent1(x)} for \code{pourcent(x, digits = 1)}, etc.
#' @export pourcent
#' @examples
#' pourcent(0.004)
#' pourcent0(c(0.000045, 0.56789, 234.567))
#' pourcent2(c(0.000045, 0.56789, 234.567))
#' pourcent5(c(0.000045, 0.56789, 234.567))

pourcent <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(paste0(format(
      round(100*x, digits = digits), 
      nsmall = digits, big.mark = " ", 
      scientific = FALSE, decimal.mark = ","
      ), " %"))
}

#' @rdname pourcent
#' @export
pourcent0 <- function (x)
{
  return(pourcent(x, digits = 0))
}

#' @rdname pourcent
#' @export
pourcent1 <- function (x)
{
  return(pourcent(x, digits = 1))
}

#' @rdname pourcent
#' @export
pourcent2 <- function (x)
{
  return(pourcent(x, digits = 2))
}

#' @rdname pourcent
#' @export
pourcent3 <- function (x)
{
  return(pourcent(x, digits = 3))
}

#' @rdname pourcent
#' @export
pourcent4 <- function (x)
{
  return(pourcent(x, digits = 5))
}

#' @rdname pourcent
#' @export
pourcent5 <- function (x)
{
  return(pourcent(x, digits = 5))
}

#' @rdname pourcent
#' @details \code{comp.pourcent(x)} returns the complement to 1 of \code{x}, 
#' i.e. \code{pourcent(1 - x)}.
#' @examples 
#' comp.pourcent(0.255)
#' @export
comp.pourcent <- function (x, digits=1) 
{
  return(pourcent(1 - x, digits))
}

#' @rdname pourcent
#' @export
comp.pourcent0 <- function (x)
{
  return(comp.pourcent(x, digits = 0))
}

#' @rdname pourcent
#' @export
comp.pourcent1 <- function (x)
{
  return(comp.pourcent(x, digits = 1))
}

#' @rdname pourcent
#' @export
comp.pourcent2 <- function (x)
{
  return(comp.pourcent(x, digits = 2))
}

#' @rdname pourcent
#' @export
comp.pourcent3 <- function (x)
{
  return(comp.pourcent(x, digits = 3))
}

#' @rdname pourcent
#' @export
comp.pourcent4 <- function (x)
{
  return(comp.pourcent(x, digits = 5))
}

#' @rdname pourcent
#' @export
comp.pourcent5 <- function (x)
{
  return(comp.pourcent(x, digits = 5))
}

#' French formatter
#' 
#' Use a comma as decimal separator and a space as separator for thousands (French way).
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{f0(x)} is a shortcut for \code{f(x, digits = 0)}, 
#' \code{f1(x)} for \code{f(x, digits = 1)}, etc.
#' @export f
#' @examples
#' f(0.004)
#' f0(c(0.000045, 0.56789, 234.567))
#' f2(c(0.000045, 0.56789, 234.567))
#' f5(c(0.000045, 0.56789, 234.567))

f <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(format(
      round(x, digits = digits), 
      nsmall = digits, big.mark = " ", 
      scientific = FALSE, decimal.mark = ","
    ))
}

#' @rdname f
#' @export
f0 <- function (x)
{
  return(f(x, digits = 0))
}

#' @rdname f
#' @export
f1 <- function (x)
{
  return(f(x, digits = 1))
}

#' @rdname f
#' @export
f2 <- function (x)
{
  return(f(x, digits = 2))
}

#' @rdname f
#' @export
f3 <- function (x)
{
  return(f(x, digits = 3))
}

#' @rdname f
#' @export
f4 <- function (x)
{
  return(f(x, digits = 5))
}

#' @rdname f
#' @export
f5 <- function (x)
{
  return(f(x, digits = 5))
}


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
#' @export
#' @examples 
#' who.format(c(3, 8, 42, 75, 45678, 9876543, 12345678))
who.format <- function(x) {
  x[x < 4 & !is.na(x)] <- 0
  x[x >= 5 & x < 10 & !is.na(x)] <- 10
  x[x >= 10 & x < 100 & !is.na(x)] <- signif(x[x >= 10 & x < 100 & !is.na(x)], digits=1)
  x[x >= 100 & x < 10000000 & !is.na(x)] <- signif(x[x >= 100 & x < 10000000 & !is.na(x)], digits=2)
  x[x >= 10000000 & !is.na(x)] <- signif(x[x >= 10000000 & !is.na(x)], digits=3)
  
  return(format(x, digits=0, big.mark=" ", scientific=FALSE))
}

