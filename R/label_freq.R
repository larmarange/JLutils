#' Cross tabulation with SPSS/Stata value labels
#' 
#' Create a contingency table (with \code{\link[stats]{xtabs}}) and add value and variable
#' labels if they are available (typically if the data frame has been imported with
#' \code{\link[foreign]{read.spss}} or \code{\link[foreign]{read.dta}})
#' 
#' @param labeled_x a formula (see \code{\link[stats]{xtabs}}) or a vector
#' @param labeled_d a data.frame (if \code{labeled_x} is a formula) or an optional second vector
#' @param add.variable.labels add variable labels to dimension names?
#' @param ... additional parameters sent to \code{\link[stats]{xtabs}}
#' 
#' @details This function is particulary useful when the data has been imported
#' from SPSS or Stata but the labeled variables not converted to factors.
#' 
#' It's possible to enter \code{ltabs(df$Sex)} for convenient purpose. In this
#' situation, the function will try to guess what is the corresponding data frame
#' (i.e. \code{df}) by analysing the function call. To avoid any issue when 
#' programming, you should prefer \code{ltabs(~Sex, df)}.
#' 
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{lfreq}}, \code{\link{lfactor}}
#' 
#' @examples 
#' \dontrun{
#'    ltabs(~Sex, df)
#'    ltabs(~Sex + Age, df)
#'    ltabs(~Sex + Age + Educ, df)
#'    ltabs(df$Sex)
#'    ltabs(df$Sex, df$Age)
#' }
#' 
#' @export

ltabs <- function(labeled_x, labeled_df = NULL, add.variable.labels = FALSE, ...) {
  if (inherits(labeled_x, "formula")) {
    if(!is.data.frame(labeled_df)) stop("When labeled_x is a formula, labeled_df should be a data.frame.")
    vars <- attr(terms(labeled_x), "term.labels")
    df <- labeled_df
    f <- labeled_x
    for (var in vars)
      if (!var %in% names(df))
        stop(paste(var, "not found in labeled_df"))
  } else {
    vars <- NULL
    dx <- deparse(substitute(labeled_x))
    posx <- regexpr("[$]", dx)[1]
    
    if (is.null(labeled_df)) {
      if(posx > 1) {
        df <- get(substr(dx, 1, posx-1))
        vars <- substr(dx, posx+1, nchar(dx))
        f <- paste("~", vars)
      } else {
        df <- NULL
        f <- paste("~", dx)
      }
    } else {
      dd <- deparse(substitute(labeled_df))
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
  
for (var in vars) df[[var]] <- lfactor(var, df)
  
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

#' Frequency table with SPSS/Stata value labels
#' 
#' This function display a frequency table with value labels if they are available
#' 
#' @param labeled_x a vector
#' @param ... additional arguments sent to \code{\link[questionr]{freq}}
#' 
#' @details This function is particulary useful when the data has been imported
#' from SPSS or Stata but the labeled variables not converted to factors.
#' 
#' \code{lfreq(df$Sex)} has been designed for convenient purpose. In this
#' situation, the function will try to guess what is the corresponding data frame
#' (i.e. \code{df}) by analysing the function call. To avoid any issue when 
#' programming, you should prefer \code{freq(ltabs(~Sex, df))}.
#' 
#' @note This function is an equivalent of applying \code{\link[questionr]{freq}} on
#' the result of \code{\link{ltabs}}. It requires the questionr package.
#' 
#' @seealso \code{\link{ltabs}}, \code{\link{lfactor}}
#' 
#' @examples 
#' \dontrun{
#'    lfreq(df$Sex)
#' }
#' 
#' @export

lfreq <- function(labeled_x, ...) {
  if (!require(questionr)) stop("questionr package is required")
  dx <- deparse(substitute(labeled_x))
  posx <- regexpr("[$]", dx)[1]
  if (posx > 1) {
    df <- get(substr(dx, 1, posx-1))
    var <- substr(dx, posx+1, nchar(dx))
    f <- as.formula(paste("~", var))
    return(freq(ltabs(f, df), ...))
  } else {
    return(freq(labeled_x, ...))
  }
}

#' Labeled factor
#' 
#' Convert to a factor using SPSS/Stata value labels if they are available
#' in the data frame
#' 
#' @param labeled_x the name of a column or a vector
#' @param labeled_df a data frame (if \code{labeled_x} is the column name)
#' @param add.codes add the numerical code to the labels?
#' 
#' @details This function is particulary useful when the data has been imported
#' from SPSS or Stata but the labeled variables not converted to factors.
#' 
#' It's possible to enter \code{lfactor(df$Sex)} for convenient purpose. In this
#' situation, the function will try to guess what is the corresponding data frame
#' (i.e. \code{df}) by analysing the function call. To avoid any issue when 
#' programming, you should prefer \code{lfactor("Sex", df)}.
#' 
#' @return If \code{labeled_x} is already a factor, \code{labeled_x} is
#' returned with no change.
#' 
#' @seealso \code{\link{ltabs}}, \code{\link{lfreq}}
#' 
#' @examples 
#' \dontrun{
#'    lfactor("Sex", df)
#'    lfactor(df$Sex)
#' }
#' 
#' @export

lfactor <- function(labeled_x, labeled_df = NULL, add.codes = TRUE) {
  if (length(labeled_x)>1) {
    dx <- deparse(substitute(labeled_x))
    posx <- regexpr("[$]", dx)[1]
    if (posx > 1) {
      labeled_df <- get(substr(dx, 1, posx-1))
      labeled_x <- substr(dx, posx+1, nchar(dx))
    } else {
      return(labeled_x)
    }
  }
  
  if (!is.data.frame(labeled_df)) stop("labeled_df should be a data frame.")
  
  if (is.factor(labeled_df[[labeled_x]])) return (labeled_df[[labeled_x]])
  
  lab <- NULL
  if (!is.null(attr(labeled_df[[labeled_x]], "value.labels")))
    lab <- attr(labeled_df[[labeled_x]], "value.labels")
  if (!is.null(attr(labeled_df, "label.table")[[labeled_x]]))
    lab <- attr(labeled_df, "label.table")[[labeled_x]]
  
  if (!is.null(lab)) {
    tmp <- data.frame(values = na.rm(unique(labeled_df[[labeled_x]])))
    tmp <- merge(tmp, data.frame(values = lab, vlab = names(lab)), all.x = TRUE)
    if (add.codes)
      tmp$flab <- paste0("[", tmp$values, "] ", tmp$vlab)
    else
      tmp$flab <- tmp$vlab
    tmp[is.na(tmp$vlab),"flab"] <- as.character(tmp[is.na(tmp$vlab),"values"])
    
    res <- factor(labeled_df[[labeled_x]], levels = tmp$values, labels = tmp$flab)
    return (res)
  } else {
    return(factor(labeled_df[[labeled_x]]))
  } 
}

