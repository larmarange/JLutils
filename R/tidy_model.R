#' A generic tidier for model outputs
#'
#' @param x a model object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates and confidence intervals
#' @param quick	whether to compute a smaller and faster version, containing only the term and estimate columns.
#' @param ... extra arguments
#' @importFrom stats coef
#' @export

tidy_model <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  if (quick) {
    co <- coef(x)
    ret <- data.frame(term = names(co), estimate = unname(co), stringsAsFactors = FALSE)
    return(process_model(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  }
  co <- coef(summary(x))
  nn <- c("estimate", "std.error", "statistic", "p.value")
  ret <- broom:::fix_data_frame(co, nn[1:ncol(co)])
  process_model(ret, x, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate)
}


process_model <- function(ret, x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE) {
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
    ret <- merge(ret, unrowname(CI), by = "term", all.x = TRUE)
  }

  ret$estimate <- trans(ret$estimate)
  ret
}

#' @rdname tidy_model
#' @export
tidy.clm <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  tidy_model(x, conf.int, conf.level, exponentiate, quick, ...)
}

#' @rdname tidy_model
#' @export
tidy.clmm <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  tidy_model(x, conf.int, conf.level, exponentiate, quick, ...)
}

#' @rdname tidy_model
#' @export
tidy.svyolr <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  tidy_model(x, conf.int, conf.level, exponentiate, quick, ...)
}
