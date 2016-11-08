#' Percent formatter (English)
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
  return(percent(x, digits = 4))
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
  return(comp.percent(x, digits = 4))
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
      nsmall = digits, big.mark = " ", 
      scientific = FALSE, decimal.mark = ","
      ), " %"))
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
  return(pourcent(x, digits = 4))
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
  return(comp.pourcent(x, digits = 4))
}

#' @rdname pourcent
#' @export
comp.pourcent5 <- function (x)
{
  return(comp.pourcent(x, digits = 5))
}

#' English formatter
#' 
#' Use a dot as decimal separator and a single quote as separator for thousands.
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{en0(x)} is a shortcut for \code{en(x, digits = 0)}, 
#' \code{en1(x)} for \code{en(x, digits = 1)}, etc.
#' @export en
#' @examples
#' en(12345.67)
#' en0(c(0.123, 123.4567, 1234567.89))
#' en2(c(0.123, 123.4567, 1234567.89))
#' en5(c(0.123, 123.4567, 1234567.89))

en <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(format(
      round(x, digits = digits), 
      nsmall = digits, big.mark = "'", 
      scientific = FALSE, decimal.mark = "."
    ))
}

#' @rdname en
#' @export
en0 <- function (x)
{
  return(en(x, digits = 0))
}

#' @rdname en
#' @export
en1 <- function (x)
{
  return(en(x, digits = 1))
}

#' @rdname en
#' @export
en2 <- function (x)
{
  return(en(x, digits = 2))
}

#' @rdname en
#' @export
en3 <- function (x)
{
  return(en(x, digits = 3))
}

#' @rdname en
#' @export
en4 <- function (x)
{
  return(en(x, digits = 4))
}

#' @rdname en
#' @export
en5 <- function (x)
{
  return(en(x, digits = 5))
}


#' French formatter
#' 
#' Use a comma as decimal separator and a space as separator for thousands.
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{fr0(x)} is a shortcut for \code{fr(x, digits = 0)}, 
#' \code{fr1(x)} for \code{fr(x, digits = 1)}, etc.
#' @export fr
#' @examples
#' fr(12345.57)
#' fr0(c(0.123, 123.4567, 1234567.89))
#' fr2(c(0.123, 123.4567, 1234567.89))
#' fr5(c(0.123, 123.4567, 1234567.89))

fr <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(format(
      round(x, digits = digits), 
      nsmall = digits, big.mark = " ", 
      scientific = FALSE, decimal.mark = ","
    ))
}

#' @rdname fr
#' @export
fr0 <- function (x)
{
  return(fr(x, digits = 0))
}

#' @rdname fr
#' @export
fr1 <- function (x)
{
  return(fr(x, digits = 1))
}

#' @rdname fr
#' @export
fr2 <- function (x)
{
  return(fr(x, digits = 2))
}

#' @rdname fr
#' @export
fr3 <- function (x)
{
  return(fr(x, digits = 3))
}

#' @rdname fr
#' @export
fr4 <- function (x)
{
  return(fr(x, digits = 4))
}

#' @rdname fr
#' @export
fr5 <- function (x)
{
  return(fr(x, digits = 5))
}

#' International formatter
#' 
#' Use a dot as decimal separator and a space as separator for thousands.
#' 
#' @param x a numeric vector to format
#' @param digits number of decimal digits to display
#' @details \code{int0(x)} is a shortcut for \code{int(x, digits = 0)}, 
#' \code{int1(x)} for \code{int(x, digits = 1)}, etc.
#' @export int
#' @examples
#' int(12345.67)
#' int0(c(0.123, 123.4567, 1234567.89))
#' int2(c(0.123, 123.4567, 1234567.89))
#' int5(c(0.123, 123.4567, 1234567.89))

int <- function (x, digits=1) 
{
  if (all(is.na(x)))
    return("NA")
  else
    return(format(
      round(x, digits = digits), 
      nsmall = digits, big.mark = " ", 
      sciinttific = FALSE, decimal.mark = "."
    ))
}

#' @rdname int
#' @export
int0 <- function (x)
{
  return(int(x, digits = 0))
}

#' @rdname int
#' @export
int1 <- function (x)
{
  return(int(x, digits = 1))
}

#' @rdname int
#' @export
int2 <- function (x)
{
  return(int(x, digits = 2))
}

#' @rdname int
#' @export
int3 <- function (x)
{
  return(int(x, digits = 3))
}

#' @rdname int
#' @export
int4 <- function (x)
{
  return(int(x, digits = 4))
}

#' @rdname int
#' @export
int5 <- function (x)
{
  return(int(x, digits = 5))
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
#' 
#' @param x a numeric vector to format
#' 
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

