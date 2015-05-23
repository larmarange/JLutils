#' Labelled table
#' 
#' Create a contingency table (with \code{\link[stats]{xtabs}}) and add value and variable
#' labels if they are available (typically if the data frame has been imported with
#' \code{\link[foreign]{read.spss}} or \code{\link[foreign]{read.dta}})
#' 
#' @param x a formula (see \code{\link[stats]{xtabs}}) or a vector
#' @param d a data.frame (if \code{x} is a formula) or an optional second vector
#' @param add.variable.labels add variable labels to dimension names?
#' @param ... additional parameters sent to \code{\link[stats]{xtabs}}
#' 
#' @details This function is particulary useful when the data has been imported
#' from SPSS or Stata but the labelled variables not converted to factors.
#' 
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{lfreq}}
#' 
#' @examples 
#' \dontrun{
#'    ltabs(~Sex, df)
#'    ltabs(~Sex+Age, df)
#'    ltabs(df$Sex)
#'    ltabs(df$Sex, df$Age)
#' }
#' 
#' @export

ltabs <- function(x, d = NULL, add.variable.labels = FALSE, ...) {
  if (inherits(x, "formula")) {
    if(!is.data.frame(d)) stop("When x is a formula, d should be a data.frame.")
    vars <- attr(terms(x), "term.labels")
    df <- d
    f <- x
  } else {
    vars <- NULL
    dx <- deparse(substitute(x))
    posx <- regexpr("[$]", dx)[1]
    
    if (is.null(d)) {
      if(posx > 1) {
        df <- get(substr(dx, 1, posx-1))
        vars <- substr(dx, posx+1, nchar(dx))
        f <- paste("~", vars)
      } else {
        df <- NULL
        f <- paste("~", dx)
      }
    } else {
      dd <- deparse(substitute(d))
      posd <- regexpr("[$]", dd)[1]
      
      if (posx > 1 & posd > 1 & substr(dx, 1, posx-1) == substr(dd, 1, posd-1)) { ## If same data.frame
        df <- get(substr(dx, 1, posx-1))
        vars <- c(substr(dx, posx+1, nchar(dx)), substr(dd, posd+1, nchar(dd)))
        f <- paste("~", paste(vars, collapse = "+"))
      } else {
        df <- NULL
        f <- paste("~", dx, "+", dd)
      }
    }
  }
  
  for (var in vars) df[[var]] <- .labelling_var(var, df)
  
  tab <- xtabs(f, df, ...)
  
  if (add.variable.labels & is.data.frame(df) & !is.null(vars)) {
    var.labels <- NULL
    if (!is.null(attr(df, "variable.labels"))) var.labels <- attr(df, "variable.labels")
    if (!is.null(attr(df, "var.labels"))) var.labels <- attr(df, "var.labels")
    if (!is.null(var.labels)) 
      names(dimnames(tab)) <- paste(vars, var.labels[pmatch(vars, names(df))], sep = ": ")
  }
  
  return(tab)
}

#' Frequency table with value labels
#' 
#' This function display a frequency table with value labels if they are available
#' 
#' @param x a vector
#' @param ... additional arguments sent to \code{\link[questionr]{freq}}
#' 
#' @details This function is particulary useful when the data has been imported
#' from SPSS or Stata but the labelled variables not converted to factors.
#' 
#' @note This function is an equivalent of applying \code{\link[questionr]{freq}} on
#' the result of \code{\link{ltabs}}.
#' 
#' @seealso \code{\link{ltabs}}
#' 
#' @examples 
#' \dontrun{
#'    lfreq(df$Sex)
#' }
#' 
#' @export

lfreq <- function(x, ...) {
  if (!require(questionr)) stop("questionr package is required")
  dx <- deparse(substitute(x))
  posx <- regexpr("[$]", dx)[1]
  if (posx > 1) {
    df <- get(substr(dx, 1, posx-1))
    var <- substr(dx, posx+1, nchar(dx))
    f <- as.formula(paste("~", var))
    return(freq(ltabs(f, df), ...))
  } else {
    return(freq(x, ...))
  }
}


.labelling_var <- function(var, df = NULL) {
  require(questionr)
  if (is.null(df) & is.factor(var)) return (var)
  
  if (is.factor(df[[var]])) return (df[[var]])
  
  if (!is.null(attr(df[[var]], "value.labels")))
    lab <- attr(df[[var]], "value.labels")
  if (!is.null(attr(df, "label.table")[[var]]))
    lab <- attr(df, "label.table")[[var]]
  
  if (!is.null(lab)) {
    tmp <- data.frame(values = na.rm(unique(df[[var]])))
    tmp <- merge(tmp, data.frame(values = lab, vlab = names(lab)), all.x = TRUE)
    tmp$flab <- paste0("[", tmp$values, "] ", tmp$vlab)
    tmp[is.na(tmp$vlab),"flab"] <- as.character(tmp[is.na(tmp$vlab),"values"])
    
    res <- factor(df[[var]], levels = tmp$values, labels = tmp$flab)
    return (res)
  } else {
    return(factor(df[[var]]))
  }
}

