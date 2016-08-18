#' Cross-tabulation matrix plot
#' 
#' By default, plot chi-squared residuals
#' 
#' @param formula formula of variables to be cross-tabulated, rows on left hand side and columns on the right hand side
#' @param data data frame containing the data
#' @param weight optionnal string indicating a column containing weights
#' @param addNA whether to include NA values in the tables
#' @param fill variable mapped to fill OR color name
#' @param fill_breaks how to cut fill variable into categories? (cf. \code{\link[base]{cut}})
#' @param palette Brewer colour palette (see \url{http://colorbrewer2.org})
#' @param fill_title legend title for fill
#' @param size variable mapped to size
#' @param max_size size of largest point
#' @param size_title legend title for size
#' @param label optionnal cell labels (see examples)
#' @param label_size size of cell labels
#' @param return_data return computed data.frame instead of ggplot?
#' @return 
#' a ggplot graphic or a data frame if \code{return_data == TRUE}.
#' @examples 
#' ggcross(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq")
#' ggcross(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", return_data = TRUE)
#' ggcross(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", label = "scales::percent(row.prop)")
#' ggcross(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", breaks = c(-4, -2, 0, 2, 4))
#' @export
ggcross <- function(
  formula, data, weight = NULL, addNA = FALSE, 
  fill = "residuals", fill_breaks = c(-4, -2, 2, 4), palette = "RdBu", fill_title = "Pearson residuals",
  size = "observed", max_size = 20, size_title = "Observations", 
  labels = NULL, labels_size = 3.5, 
  return_data = FALSE) 
{
  if (!is.data.frame(data))
    data <- as.data.frame(data)
  if (!is.formula(formula))
    formula <- as.formula(formula)
  if (!all(all.vars(formula) %in% names(data)))
    stop("all specified variables should be in data.")
  if (!is.null(weight))
    if (!weight %in% names(data))
      stop("weight variable should be in data.")
  row.vars <- all.vars(formula[[2]])
  col.vars <- all.vars(formula[[3]])
  
  if (addNA)
    data <- lapply(data, factor, exclude = NULL) 
  
  require(plyr, quietly = TRUE)
  res <- data.frame()
  for (rv in row.vars) {
    for (cv in col.vars) {
      if (cv != rv) {
        if (is.null(weight))
          f <- as.formula(paste("~", rv, "+", cv))
        else
          f <- as.formula(paste(weight, "~", rv, "+", cv))
        tmp <- .tidy_chisq(chisq.test(xtabs(f, data)))
        names(tmp)[1:2] <- c("row.label", "col.label")
        tmp$row.variable <- rv
        tmp$col.variable <- cv
        res <- rbind.fill(res, tmp)
      }
    }
  }
  
  fill_var <- NULL
  fill_col <- NULL
  if (!is.null(fill)) {
    if (fill %in% names(res)) {
      if (length(fill_breaks) > 1) {
        if (min(res[[fill]]) < min(fill_breaks))
          fill_breaks <- c(min(res[[fill]]), fill_breaks)
        if (max(res[[fill]]) > max(fill_breaks))
          fill_breaks <- c(fill_breaks, max(res[[fill]]))
      }
      res$category <- cut(res[[fill]], include.lowest = TRUE,  right = FALSE, breaks = fill_breaks, dig.lab = 10)
      fill_var <- "category"
    } else {
      fill_col <- fill
    }
  } else {
    fill_col <- "grey"  
  }

  if (return_data)
    return(res)
  
  require(ggplot2, quietly = TRUE)
  
  if (is.null(size))
    size <- 1
  
  res$row.label <- factor(res$row.label, rev(levels(res$row.label)))
  p <- ggplot(res) + 
    aes_string(x = "col.label", y = "row.label", fill = fill_var)
  
  if (is.null(fill_col))
    p <- p + geom_point(aes_string(size = size), shape = 22, colour = "black")
  else
    p <- p + geom_point(aes_string(size = size), shape = 22, colour = "black", fill = fill_col)
  
  p <- p +
    scale_size_area(max_size = max_size) +
    facet_grid("row.variable ~ col.variable", scales = "free", space = "free") +
    xlab("") + ylab("") +
    scale_fill_brewer(palette = palette, drop = FALSE) +
    labs(fill = fill_title, size = size_title) +
    guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = max_size / 2))) +
    coord_equal()
  
  if (size == 1)
    p <- p + guides(size = FALSE)
  
  if (!is.null(labels))
    p <- p + geom_text(aes_string(label = labels), size = labels_size)

  p
}

.tidy_chisq <- function(x) {
  d <- length(dimnames(as.table(x$observed)))
  ret <- as.data.frame(as.table(x$observed))
  names(ret)[d+1] <- "observed"
  
  ret <- cbind(ret, prop = as.data.frame(prop.table(as.table(x$observed)))[[d+1]])
  if (d == 2) {
    ret <- cbind(ret, row.prop = as.data.frame(prop.table(as.table(x$observed), 1))[[d+1]])
    ret <- cbind(ret, col.prop = as.data.frame(prop.table(as.table(x$observed), 2))[[d+1]])
  }
  
  ret <- cbind(ret, expected = as.data.frame(as.table(x$expected))[[d+1]])
  ret <- cbind(ret, residuals = as.data.frame(as.table(x$residuals))[[d+1]])
  ret <- cbind(ret, stdres = as.data.frame(as.table(x$stdres))[[d+1]])
  ret
}