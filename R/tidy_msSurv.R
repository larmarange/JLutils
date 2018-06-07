#' Experimental tidier for msSurv objects
#'
#' @param x a msSurv object
#' @param extract what should be extracted?
#' @param conf.level confidence level of the intervals
#' @param conf.fun transformation appelied to confidence intervals
#' @param ... extra arguments
#' @export
tidy.msSurv <- function(
                        x, extract = c("state occupation probabilities", "transition probabilities"),
                        conf.level = 0.95,
                        conf.fun = c("linear", "log", "log-log", "cloglog"), ...) {
  extract <- match.arg(extract)
  conf.fun <- match.arg(conf.fun)
  if (!requireNamespace("msSurv", quietly = TRUE)) {
    stop("msSurv package is required.")
  }

  if (extract == "state occupation probabilities") {
    ret <- as.table(ps(x))
    names(ret)
    dimnames(ret)[[2]] <- nodes(tree(x))
    ret <- as.data.frame(ret)
    names(ret) <- c("time", "state", "estimate")
    ret <- cbind(ret, variance = as.data.frame(as.table(var.sop(x)))[[3]])
    ci <- msSurv:::MSM.CIs(x, ci.level = conf.level, trans = FALSE, sop = TRUE, ci.trans = conf.fun)
    ret <- cbind(ret, conf.low = as.data.frame(as.table(ci$CI.p[, 2, ]))[[3]])
    ret <- cbind(ret, conf.high = as.data.frame(as.table(ci$CI.p[, 3, ]))[[3]])
    return(ret)
  }

  if (extract == "transition probabilities") {
    ret <- as.data.frame(as.table(AJs(x)))
    names(ret) <- c("from", "to", "time", "estimate")
    ret$trans <- paste(ret$from, ret$to, sep = " ")
    ret <- ret[ret$trans %in% pos.trans(x), ]
    if (!is.null(cov.AJs(x))) {
      ci <- msSurv:::MSM.CIs(x, ci.level = conf.level, trans = TRUE, sop = FALSE, ci.trans = conf.fun)
      tmp <- as.data.frame(as.table(ci$CI.trans[, 4, ]))
      names(tmp) <- c("time", "trans", "variance")
      ret <- merge(ret, tmp, by = c("trans", "time"), all.x = TRUE)
      tmp <- as.data.frame(as.table(ci$CI.trans[, 2, ]))
      names(tmp) <- c("time", "trans", "conf.low")
      ret <- merge(ret, tmp, by = c("trans", "time"), all.x = TRUE)
      tmp <- as.data.frame(as.table(ci$CI.trans[, 3, ]))
      names(tmp) <- c("time", "trans", "conf.high")
      ret <- merge(ret, tmp, by = c("trans", "time"), all.x = TRUE)
    }
    # n.risk
    tmp <- Ys(x)
    dimnames(tmp)[[2]] <- substring(dimnames(tmp)[[2]], 3)
    tmp <- as.data.frame(as.table(tmp))
    names(tmp) <- c("time", "from", "n.risk")
    ret <- merge(ret, tmp, by = c("from", "time"), all.x = TRUE)
    # n.event
    tmp <- dNs(x)
    dimnames(tmp)[[2]] <- substring(dimnames(tmp)[[2]], 4)
    tmp <- as.data.frame(as.table(tmp))
    names(tmp) <- c("time", "trans", "n.event")
    ret <- merge(ret, tmp, by = c("trans", "time"), all.x = TRUE)
    # n.remain
    tmp <- sum_dNs(x)
    dimnames(tmp)[[2]] <- substr(dimnames(tmp)[[2]], 4, nchar(dimnames(tmp)[[2]]) - 2)
    dimnames(tmp)[[2]] <- paste(dimnames(tmp)[[2]], dimnames(tmp)[[2]], sep = " ")
    tmp <- as.data.frame(as.table(tmp))
    names(tmp) <- c("time", "trans", "n.sum.event")
    ret <- merge(ret, tmp, by = c("trans", "time"), all.x = TRUE)
    ret$n.remain <- ret$n.risk - ret$n.sum.event
    return(ret)
  }
}
