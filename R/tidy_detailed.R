#' Tidy data with variable names and factor levels
#' 
#' This function extends the output of \code{\link[broom]{tidy}} function from \pkg{broom} package
#' by providing additional column indicating variables and factors names.
#' 
#' @param x model to tidy
#' @param ... extra arguments passed to \code{\link[broom]{tidy}} function from \pkg{broom} package
#' @note
#' This function requires the \pkg{broom} and \pkg{dplyr} packages.
#' @return 
#' The \code{data.frame} produced by \code{\link[broom]{tidy}} with these additional columns:
#' \describe{
#'   \item{variable}{variable name}
#'   \item{variable_label}{label of the variable (if available), variable name otherwise}
#'   \item{level}{for factors, the corresponding level}
#'   \item{level_detail}{level with indication of the reference level, eg. \code{B vs. A}}
#'   \item{label}{equal to level_detail if it's a factor, to variable_label otherwise}
#' }
#' @seealso \code{\link[broom]{tidy}} function from \pkg{broom} package
#' @source
#' This function has been adapted from Benjamin Nutter's \code{tidy_levels_labels} function
#' implemented within the \code{pixiedust} package 
#' (\url{https://github.com/nutterb/pixiedust/blob/master/R/tidy_levels_labels.R}).
#' @examples 
#' reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length * Species, data = iris)
#' tidy_detailed(reg)
#' 
#' d <- as.data.frame(Titanic)
#' reg2 <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
#' tidy_detailed(reg2, conf.int = TRUE, exponentiate = TRUE) 
#' @export
tidy_detailed <- function(x, ...) {
  if (!requireNamespace("broom")) 
    stop("broom package is required. Please install it.")
  if (!requireNamespace("dplyr")) 
    stop("dplyr package is required. Please install it.")
  res <- merge(broom::tidy(x, ...), .tidy_levels_labels(x), all.x = TRUE)
  res$label <- res$level_detail
  if (sum(res$level_detail == "", na.rm = TRUE) > 0)
    res[res$level_detail == "", "label"] <- res[res$level_detail == "", "variable_label"]
  res
}


# inspired by pixiedust:::tidy_levels_labels
# https://github.com/nutterb/pixiedust/
# variable was renamed variable
.tidy_levels_labels <- function(object, descriptors = c("term", "variable", "variable_label", "level", "level_detail"), 
  numeric_level = c("variable_label", "term", "variable")) {
  numeric_level = match.arg(numeric_level)
  lnl <- .levels_and_labels(object)
  lnl <- .level_label_interactions(lnl, broom::tidy(object), numeric_level)
  if (!"term" %in% descriptors) 
    lnl[, c("term", descriptors), drop = FALSE]
  else 
    lnl[, descriptors, drop = FALSE]
}

.levels_and_labels <- function(object, ...) {
  model_data <- stats::model.frame(object)
  NLevels <- vapply(model_data, .modelNLevels, 1)
  Levels <- lapply(model_data, .modelFriendlyLevels)
  Levels <- dplyr::bind_rows(Levels)
  Levels <- dplyr::mutate_(Levels, variable = ~rep(names(NLevels), 
    NLevels), term = ~paste0(variable, level))
  # Levels <- lapply(model_data, modelFriendlyLevels) %>%
  # dplyr::bind_rows() %>% dplyr::mutate_(variable =
  # ~rep(names(NLevels), NLevels), term = ~paste0(variable,
  # level))
  Levels$variable_label <- Levels$variable
  Labels <- NULL
  if (requireNamespace("labelled")) 
    Labels <- unlist(labelled::var_label(model_data))
  if (!is.null(Labels)) {
    Levels$variable_label <- Labels[Levels$variable]
    if (any(is.na(Levels$variable_label))) 
      Levels$variable_label[is.na(Levels$variable_label)] <- Levels$variable[is.na(Levels$variable_label)]
  }
  Levels <- Levels[, c("term", "variable", "variable_label", "level", 
    "level_detail")]
  rownames(Levels) <- NULL
  Levels
}

.modelNLevels <- function(f) {
  nlev <- nlevels(f)
  nlev <- if (nlev == 0) 
    1 else (nlev - 1)
  nlev
}

.modelFriendlyLevels <- function(f) {
  lev <- levels(f)
  if (is.null(lev)) 
    return(data.frame(level = "", level_detail = "", stringsAsFactors = FALSE)) 
  else 
    return(data.frame(level = lev[-1], level_detail = paste0(lev[-1], 
      " vs. ", lev[1]), stringsAsFactors = FALSE))
}

.level_label_interactions <- function (lnl, tidy_fit, numeric_level) 
{
  if (!any(grepl("[:]", tidy_fit$term))) 
    return(lnl)
  else {
    inters <- which(grepl("[:]", tidy_fit$term))
    splits <- strsplit(tidy_fit$term[inters], "[:]")
    inters <- lapply(splits, .form_interaction_labels, lnl, numeric_level)
    inters <- dplyr::bind_rows(inters)
    dplyr::bind_rows(lnl, inters)
  }
}

.form_interaction_labels <- function (s, lnl, numeric_level) 
{
  m <- match(s, lnl$term)
  level <- ifelse(lnl$level[m] == "", lnl[[numeric_level]][m], 
                  lnl$level[m])
  level_detail <- ifelse(lnl$level_detail[m] == "", lnl[[numeric_level]][m], 
                         lnl$level_detail[m])
  data.frame(term = paste0(lnl$term[m], collapse = ":"), variable = paste0(lnl$variable[m], 
                                                                             collapse = ":"), variable_label = paste0(lnl$variable_label[m], collapse = ":"), 
             level = paste0(level, collapse = ":"), level_detail = paste0(level_detail, 
                                                                          collapse = ":"), stringsAsFactors = FALSE)
}
