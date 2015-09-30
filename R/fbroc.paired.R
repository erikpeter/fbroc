#' Bootstrap paired ROC curve
#'
#' @inheritParams boot.roc
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x1 <- rnorm(1000) + y
#' x2 <- 0.5*x1 + 0.5*rnorm(1000) + y
#' result.boot <- boot.paired.roc(x1, x2, y)
#' @seealso \url{http://www.epeter-stats.de/roc-curves-and-ties/}, \code{\link{plot.fbroc.roc}}, 
#' \code{\link{print.fbroc.roc}}
#' 
#' @export
boot.paired.roc <- function(pred1, pred2, true.class, stratify = TRUE, n.boot = 1000,
                            use.cache = FALSE, tie.strategy = NULL) {
  # validate input
  if ((length(pred1) != length(true.class)))
    stop("Predictions and true classes need to have the same length")
  if ((length(pred2) != length(true.class)))
    stop("Predictions and true classes need to have the same length")
  if (class(pred1) == "integer") {
    pred1 <- as.numeric(pred1)
  }
  if ((class(pred1) != "numeric"))
    stop("Predictions must be numeric")
  
  if (class(pred2) == "integer") {
    pred2 <- as.numeric(pred2)
  }
  if ((class(pred2) != "numeric"))
    stop("Predictions must be numeric")
  
  if ((class(true.class) != "logical"))
    stop("Classes must be logical")
  if ((class(stratify) != "logical"))
    stop("Classes must be logical")
  
  index.na <- is.na(pred1) | is.na(pred2) | is.na(true.class)
  if (any(index.na)) {
    n <- sum(index.na)
    warning.msg <- 
      paste(n, "observations had to be removed due to missing values")
    warning(warning.msg)
    true.class <- true.class[!index.na]
    pred1 <- pred1[!index.na]
    pred2 <- pred2[!index.na]
  }
  if (is.null(tie.strategy)) {
    if ((length(pred1) == length(unique(pred1))) &
        (length(pred2) == length(unique(pred2)))) 
      tie.strategy <- 1 
    else tie.strategy <- 2
  }
  
  if (!(tie.strategy %in% 1:2)) stop("tie.strategy must be 1 or 2")
  if (sum(true.class) == 0)
    stop("No positive observations are included")
  if (sum(!true.class) == 0)
    stop("No negative observations are included")
  
  n.boot <- as.integer(n.boot)
  
  if (length(n.boot) != 1)
    stop("n.boot must have length 1")
  
  if (length(stratify) != 1)
    stop("stratify must have length 1")
  
  if (!stratify) stop("Non-stratified bootstrapping is not yet supported")
  
  true.int <- as.integer(true.class)
  original.rocs <- paired_roc_analysis(pred1, pred2, true.int)
  
  roc1 = c.list.to.roc(original.rocs[[1]])
  roc2 = c.list.to.roc(original.rocs[[2]])
  
  auc1 = original.rocs[[1]][[4]]
  auc2 = original.rocs[[2]][[4]]
  if (use.cache) {
    stop("Cached mode not enabled yet")
    booted.roc <- tpr_fpr_boot2(pred1, true.int, n.boot)
    boot.tpr <- booted.roc[[1]]
    boot.fpr <- booted.roc[[2]]
    rm(booted.roc)
  } else {
    boot.tpr1 <- NULL
    boot.fpr1 <- NULL
    boot.tpr2 <- NULL
    boot.fpr2 <- NULL
  }
  
  output <- list(predictions1 = pred1,
                 predictions2 = pred2,
                 true.classes = true.class,
                 n.thresholds1 = nrow(roc1),
                 n.thresholds2 = nrow(roc2),
                 n.boot = as.integer(n.boot),
                 use.cache = use.cache,
                 tie.strategy = tie.strategy,
                 n.pos = sum(true.class),
                 n.neg = sum(!true.class),
                 roc1 = roc1,
                 roc2 = roc2,
                 auc1 = auc1,
                 auc2 = auc2,
                 #auc = auc,
                 boot.tpr1 = boot.tpr1,
                 boot.fpr1 = boot.fpr1,
                 boot.tpr2 = boot.tpr2,
                 boot.fpr2 = boot.fpr2)
  class(output) <- append(class(output), "fbroc.paired.roc")
  return(output)
}

#' BlaBla
#' 
#' Placeholder
#' @inheritParams conf.roc
#' @return A data.frame containing the FPR steps and the lower and upper bounds
#' of the confidence interval for the TPR.
#' @export
#' @seealso \code{\link{boot.roc}}
conf.roc.paired <- function(roc, conf.level = 0.95, conf.for = "tpr", steps = 250) {
  
  if (!(conf.for %in% c("tpr", "fpr"))) stop("Invalid rate given for confidence region")
  
  alpha <- 0.5*(1 - conf.level)
  alpha.levels <- c(alpha, 1 - alpha) 
  steps = as.integer(steps)
  # translate tpr_fpr at threshold matrix into tpr at fpr matrix
  if (conf.for == "tpr") {
    if (roc$use.cache) {
      stop("Cached not implemented yet")
      #rel.matrix <- tpr_at_fpr_cached(roc$boot.tpr, roc$boot.fpr, roc$n.thresholds, steps)
    } else {
      rel.matrix <- tpr_at_fpr_delta_uncached(roc$predictions1,
                                              roc$predictions2,
                                              as.integer(roc$true.classes),
                                              roc$n.boot,
                                              steps)
    }
  }
  
  # translate tpr_fpr at threshold matrix into tpr at fpr matrix
  if (conf.for == "fpr") {
    if (roc$use.cache) {
      stop("Cached not implemented yet")
      #rel.matrix <- tpr_at_fpr_cached(roc$boot.tpr, roc$boot.fpr, roc$n.thresholds, steps)
    } else {
      rel.matrix <- fpr_at_tpr_delta_uncached(roc$predictions1,
                                              roc$predictions2,
                                              as.integer(roc$true.classes),
                                              roc$n.boot,
                                              steps)
    }
  }
  
  rm(roc)
  conf.area <- t(apply(rel.matrix, 2, quantile, alpha.levels))
  conf.area <- as.data.frame(conf.area)
  if (conf.for == "tpr") {
    names(conf.area) <- c("Lower.Delta.TPR", "Upper.Delta.TPR")
    conf.area <- cbind(data.frame(FPR = 1 - seq(0, 1, by = (1 / steps))), conf.area)
  } else {
    names(conf.area) <- c("Lower.Delta.FPR", "Upper.Delta.FPR")
    conf.area <- cbind(data.frame(TPR = 1 - seq(0, 1, by = (1 / steps))), conf.area)
  }
  return(conf.area)
}


