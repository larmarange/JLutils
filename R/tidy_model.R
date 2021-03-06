#' A generic tidier for model outputs
#' 
#' \lifecycle{experimental}
#'
#' @param x a model object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates and confidence intervals
#' @param quick	whether to compute a smaller and faster version, containing only the term and estimate columns.
#' @param ... extra arguments
#' @importFrom stats coef
#' @importFrom broom tidy
#' @export

tidy_model <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  if (quick) {
    co <- coef(x)
    ret <- data.frame(term = names(co), estimate = unname(co), stringsAsFactors = FALSE)
    return(.process_model(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  }
  co <- coef(summary(x))
  nn <- c("estimate", "std.error", "statistic", "p.value")
  ret <- .fix_data_frame(co, nn[1:ncol(co)])
  .process_model(ret, x, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate)
}


.process_model <- function(ret, x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE) {
  if (exponentiate) {
    trans <- exp
  } else {
    trans <- identity
  }

  if (conf.int) {
    # avoid "Waiting for profiling to be done..." message
    CI <- suppressMessages(trans(stats::confint(x, level = conf.level)))
    colnames(CI) <- c("conf.low", "conf.high")
    CI <- as.data.frame(CI)
    CI$term <- rownames(CI)
    ret <- merge(ret, .unrowname(CI), by = "term", all.x = TRUE)
  }

  ret$estimate <- trans(ret$estimate)
  ret
}


.fix_data_frame<- function (x, newnames = NULL, newcol = "term") 
{
  if (!is.null(newnames) && length(newnames) != ncol(x)) {
    stop("newnames must be NULL or have length equal to number of columns")
  }
  if (all(rownames(x) == seq_len(nrow(x)))) {
    ret <- data.frame(x, stringsAsFactors = FALSE)
    if (!is.null(newnames)) {
      colnames(ret) <- newnames
    }
  }
  else {
    ret <- data.frame(...new.col... = rownames(x), .unrowname(x), 
                      stringsAsFactors = FALSE)
    colnames(ret)[1] <- newcol
    if (!is.null(newnames)) {
      colnames(ret)[-1] <- newnames
    }
  }
  tibble::as_tibble(ret)
}

.unrowname <- function(x) {
  rownames(x) <- NULL
  x
}