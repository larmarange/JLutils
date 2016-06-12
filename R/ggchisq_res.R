#' Chi-squared residuals matrix plot
#' 
#' @param formula formula of variables to be cross-tabulated, rows on left hand side and columns on the right hand side
#' @param data data frame containing the data
#' @param weight optionnal string indicating a column containing weights
#' @param addNA whether to include NA values in the tables
#' @param label optionnal cell labels (see examples)
#' @param breaks how to recode residuals into categories?
#' @param palette Brewer colour palette (see \url{http://colorbrewer2.org})
#' @param return_data return computed data.frame instead of ggplot?
#' @return 
#' a ggplot graphic or a data frame if \code{return_data == TRUE}.
#' @examples 
#' ggchisq_res(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq")
#' ggchisq_res(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", return_data = TRUE)
#' ggchisq_res(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", label = "scales::percent(row.prop)")
#' ggchisq_res(Sex + Age + Class ~ Survived, data = as.data.frame(Titanic), weight = "Freq", breaks = c(-4, -2, 0, 2, 4))
#' @export
ggchisq_res <- function(
  formula, data, 
  weight = NULL, addNA = FALSE, 
  label = NULL, breaks = c(-4, -2, 2, 4), 
  palette = "RdBu", return_data = FALSE) 
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
        tmp <- tidy_chisq(chisq.test(xtabs(f, data)))
        names(tmp)[1:2] <- c("row.label", "col.label")
        tmp$row.variable <- rv
        tmp$col.variable <- cv
        res <- rbind.fill(res, tmp)
      }
    }
  }
  
  if (min(res$residuals) < min(breaks))
    breaks <- c(min(res$residuals), breaks)
  if (max(res$residuals) > max(breaks))
    breaks <- c(breaks, max(res$residuals))
  
  res$residuals_cat <- cut(res$residuals, include.lowest = TRUE,  right = FALSE, breaks = breaks)

  if (return_data)
    return(res)
  
  require(ggplot2, quietly = TRUE)
  
  res$row.label <- factor(res$row.label, rev(levels(res$row.label)))
  p <- ggplot(res) + 
    aes_string(x = "col.label", y = "row.label", fill = "residuals_cat") +
    geom_raster() +
    facet_grid("row.variable ~ col.variable", scales = "free", space = "free") +
    xlab("") + ylab("") +
    scale_fill_brewer(palette = palette, drop = FALSE) +
    labs(fill = "Pearson residuals") +
    guides(fill = guide_legend(reverse = TRUE))
  
  if (!is.null(label))
    p <- p + geom_text(aes_string(label = label))

  p
}