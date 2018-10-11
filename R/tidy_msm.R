#'  Basic tieders for msm models
#' 
#' @param x an object returned by \code{\link[msm]{prevalence.msm}} or \code{\link[msm]{hazard.msm}}
#' @export
#' @import magrittr
#' @examples 
#' \dontrun{
#'   if (require(msm)) {
#'     twoway4.q <- rbind(c(-0.5, 0.25, 0, 0.25), c(0.166, -0.498, 0.166, 0.166),
#'     c(0, 0.25, -0.5, 0.25), c(0, 0, 0, 0))
#'     cav.msm <- msm(state ~ years, subject=PTNUM, data = cav,
#'                    qmatrix = twoway4.q, deathexact = 4, covariates = ~ sex)
#'     tidy.prevalence.msm(prevalence.msm(cav.msm))
#'     tidy.prevalence.msm(prevalence.msm(cav.msm, ci = "normal"))
#'     tidy.hazard.msm(hazard.msm(cav.msm))
#'   }
#' }
tidy.prevalence.msm <- function(x) {
  n_status <- ncol(x$Observed) - 1
  
  obs <- as.data.frame(x$Observed[, 1:n_status])
  obs$time <- rownames(obs)
  obs <- tidyr::gather(obs, -time, key = status, value = observed)
  
  if (is.matrix(x$Expected)) {
    exp <- as.data.frame(x$Expected[, 1:n_status])
    exp$time <- rownames(exp)
    exp <- tidyr::gather(exp, -time, key = status, value = expected)
  } else {
    exp <- as.data.frame(x$Expected$estimates[, 1:n_status])
    exp$time <- rownames(exp)
    exp <- tidyr::gather(exp, -time, key = status, value = expected)
    
    exp_ci_low <- x$Expected$ci[, 1:n_status, 1]
    colnames(exp_ci_low) <- colnames(x$Expected$estimates[, 1:n_status])
    exp_ci_low <- as.data.frame(exp_ci_low)
    exp_ci_low$time <- rownames(x$Expected$estimates)
    exp_ci_low <- tidyr::gather(exp_ci_low, -time, key = status, value = expected.low)
    
    exp_ci_high <- x$Expected$ci[, 1:n_status, 2]
    colnames(exp_ci_high) <- colnames(x$Expected$estimates[, 1:n_status])
    exp_ci_high <- as.data.frame(exp_ci_high)
    exp_ci_high$time <- rownames(x$Expected$estimates)
    exp_ci_high <- tidyr::gather(exp_ci_high, -time, key = status, value = expected.high)
  }
  
  obs_p <- as.data.frame(x$`Observed percentages`)
  obs_p$time <- rownames(obs_p)
  obs_p <- tidyr::gather(obs_p, -time, key = status, value = observed.percentage)
  
  exp_p <- as.data.frame(x$`Expected percentages`)
  exp_p$time <- rownames(exp_p)
  exp_p <- tidyr::gather(exp_p, -time, key = status, value = expected.percentage)
  
  if (is.matrix(x$`Expected percentages`)) {
    exp_p <- as.data.frame(x$`Expected percentages`)
    exp_p$time <- rownames(exp_p)
    exp_p <- tidyr::gather(exp_p, -time, key = status, value = expected.percentage)
  } else {
    exp_p <- as.data.frame(x$`Expected percentages`$estimates)
    exp_p$time <- rownames(exp_p)
    exp_p <- tidyr::gather(exp_p, -time, key = status, value = expected.percentage)
    
    exp_p_ci_low <- x$`Expected percentages`$ci
    colnames(exp_p_ci_low) <- colnames(x$`Expected percentages`$estimates)
    exp_p_ci_low <- as.data.frame(exp_p_ci_low)
    exp_p_ci_low$time <- rownames(x$`Expected percentages`$estimates)
    exp_p_ci_low <- tidyr::gather(exp_p_ci_low, -time, key = status, value = expected.percentage.low)
    
    exp_p_ci_high <- x$`Expected percentages`$ci
    colnames(exp_p_ci_high) <- colnames(x$`Expected percentages`$estimates)
    exp_p_ci_high <- as.data.frame(exp_p_ci_high)
    exp_p_ci_high$time <- rownames(x$`Expected percentages`$estimates)
    exp_p_ci_high <- tidyr::gather(exp_p_ci_high, -time, key = status, value = expected.percentage.high)
  }
  
  
  res <- obs %>% 
    dplyr::left_join(exp, by = c("time", "status")) %>%
    dplyr::left_join(obs_p, by = c("time", "status")) %>%
    dplyr::left_join(exp_p, by = c("time", "status")) %>%
    tibble::as.tibble()
  
  if (is.list(x$Expected))
    res <- res %>%
    dplyr::left_join(exp_ci_low, by = c("time", "status")) %>%
    dplyr::left_join(exp_ci_high, by = c("time", "status")) %>%
    dplyr::left_join(exp_p_ci_low, by = c("time", "status")) %>%
    dplyr::left_join(exp_p_ci_high, by = c("time", "status"))
  
  res$time <- as.numeric(res$time)
  
  res
}

#' @rdname tidy.prevalence.msm
#' @export
tidy.hazard.msm <- function(x) {
  res <- tibble::tibble()
  for (term in names(x)) {
    tmp <- tibble::as.tibble(x[[term]])
    names(tmp) <- c("estimate", "conf.low", "conf.high")
    tmp$term <- term
    tmp$transition <- rownames(x[[term]])
    res <- dplyr::bind_rows(res, tmp)
  }
  
  res <- res %>% tidyr::separate(transition, c("from", "to"), sep = " - ", remove = FALSE) 
  res[, c("term", "transition", "from", "to", "estimate", "conf.low", "conf.high")]
}
