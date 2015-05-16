#' Bootstrap ROC curve
#'
#' \code{boot.roc} calculates the ROC curve, initializes the settings
#' and calculates the bootstrap results for the true and false
#' positive rate at every relevant threshold. Missing values are removed with 
#' a warning prior to bootstrapping.
#'
#' @param pred A numeric vector. Contains predictions. \code{boot.roc} assumes
#'   that a high prediction is evidence for the observation belonging to the
#'   positive class.
#' @param true.class A logical vector. TRUE indicates the sample belonging to the
#'   positive class.
#' @param stratify Logical. Indicates whether we use stratified bootstrap.
#'   Default to TRUE. Non-stratified bootstrap is not yet implemented.
#' @param n.boot A number that will be coerced to integer. Specified the 
#'   number of bootstrap replicates. Defaults to 1000.
#' @param use.cache If true (default) the bootstrapping results for the
#'   ROC curve will be pre-cached. This increases both speed and memory usage.   
#' @return A list of class \code{fbroc.roc}, containing the elements:
#' \item{prediction}{Input predictions.}
#' \item{true.class}{Input classes.}
#' \item{thresholds}{Thresholds.}
#' \item{n.thresholds}{Number of thresholds.}
#' \item{n.boot}{Number of bootstrap replicates.}
#' \item{use.cache}{Indicates if cache is used for this ROC object}
#' \item{n.pos}{Number of positive observations.}
#' \item{n.neg}{Number of negative observations.}
#' \item{tpr.fpr}{Vector containing true and false positive rates at
#'                      the different thresholds for the original predictions.}
#' \item{tpr.fpr.raw}{Vector containing raw results from C++ for later usage by
#'  other functions. Will be NULL if use.cache is FALSE.}       
#' \item{time.used}{Time in seconds used for the bootstrap. Other steps are not
#' included. Will be NA if use.cache is FALSE.}
#' \item{auc}{The AUC of the original ROC curve.}
#' \item{tpr.fpr.boot.matrix}{Matrix containing TPR and FPR values at the
#' thresholds for each bootstrap replicate.}
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y)
#' @seealso \code{\link{plot.fbroc.roc}}, \code{\link{print.fbroc.roc}}
#' @export
boot.roc <- function(pred, true.class, stratify = TRUE, n.boot = 1000,
                     use.cache = TRUE) {
  # validate input
  if ((length(pred) != length(true.class)))
    stop("Predictions and true classes need to have the same length")
  if ((class(pred) != "numeric"))
    stop("Predictions must be numeric")
  if ((class(true.class) != "logical"))
    stop("Classes must be logical")
  if ((class(stratify) != "logical"))
    stop("Classes must be logical")
  
  index.na <- is.na(pred) | is.na(true.class)
  if (any(index.na)) {
    n <- sum(index.na)
    warning.msg <- 
      paste(n, "observations had to be removed due to missing values")
    warning(warning.msg)
    true.class <- true.class[!index.na]
    pred <- pred[!index.na]
  }
  
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
  
  new.order <- order(pred)
  pred <- pred[new.order]
  true.class <- true.class[new.order]
  true.int <- as.integer(true.class)

  original.roc <- roc_analysis(pred, true.int)
  auc <- original.roc[[4]]
  original.roc[[4]] <- NULL
  original.roc <- as.data.frame(original.roc)
  names(original.roc) <- c("TPR", "FPR", "threshold")
  if (use.cache) {
    booted.roc <- tpr_fpr_boot2(pred, true.int, n.boot)
    boot.tpr <- booted.roc[[1]]
    boot.fpr <- booted.roc[[2]]
    rm(booted.roc)
  } else {
    boot.tpr <- NULL
    boot.fpr <- NULL
  }
  
  output <- list(predictions = pred,
                 true.classes = true.class,
                 n.thresholds = nrow(original.roc),
                 n.boot = as.integer(n.boot),
                 use.cache = use.cache,
                 n.pos = sum(true.class),
                 n.neg = sum(!true.class),
                 roc = original.roc,
                 auc = auc,
                 boot.tpr = boot.tpr,
                 boot.fpr = boot.fpr)
  class(output) <- append(class(output), "fbroc.roc")
  return(output)
}


#' Generates confidence intervals for the TPR for a range of FPRs
#' 
#' Calculates confidence intervals for the TPR at different FPR values. 
#' This function is also used to plot the confidence regions in the
#' function \code{\link{plot.fbroc.roc}}.
#' 
#' @param roc Object of class \code{fbroc.roc}.
#' @param conf.level Confidence level to be used for the confidence intervals.
#' @param steps Number of discrete steps for the FPR at which the TPR is 
#' calculated. TPR confidence intervals are given for all FPRs in 
#' \code{seq(0, 1, by = (1 / steps))}.
#' @return A data.frame containing the FPR steps and the lower and upper bounds
#' of the confidence interval for the TPR.
#' @export
#' @seealso \code{\link{boot.roc}}
conf.roc <- function(roc, conf.level = 0.95, steps = 200) {
  alpha <- 0.5*(1 - conf.level)
  alpha.levels <- c(alpha, 1 - alpha) 
  steps = as.integer(steps)
  # translate tpr_fpr at threshold matrix into tpr at fpr matrix
  if (roc$use.cache) {
    rel.matrix <- tpr_at_fpr_cached(roc$boot.tpr, roc$boot.fpr, roc$n.thresholds, steps)
  } else {
    rel.matrix <- tpr_at_fpr_uncached(roc$predictions,
                                      as.integer(roc$true.classes),
                                      roc$n.boot,
                                      steps)
  }
  rm(roc)
  conf.area <- t(apply(rel.matrix, 2, quantile, alpha.levels))
  conf.area <- as.data.frame(conf.area)
  names(conf.area) <- c("Lower.TPR", "Upper.TPR")
  conf.area <- cbind(data.frame(FPR = 1 - seq(0, 1, by = (1 / steps))), conf.area)
  return(conf.area)
}

#' Process bootstrapped TPR/FPR at thresholds matrix into TPR at FPR matrix
#' 
#' Function \code{boot.roc} contains the TPR and FPR result at each threshold
#' per bootstrap replicate. This is easy to calculate, but often not convenient
#' to work with. Therefore \code{boot.tpr.at.fpr} transform that matrix
#' so that in each column are the bootstrap results for the TPR at a specific
#' FPR.
#' @param roc Object of class \code{fbroc.roc}.
#' @param steps Number of discrete steps for the FPR at which the TPR is 
#' calculated. TPR confidence intervals are given for all FPRs in 
#' \code{seq(0, 1, by = (1 / steps))}.
#' @return Matrix containing the TPR bootstrap replicates for the discrete
#' FPR steps.
#' @export
#' @seealso \code{\link{boot.roc}}
boot.tpr.at.fpr <- function(roc, steps) {
  steps = as.integer(steps)
  rel.matrix <- get_tpr_matrix(roc$tpr.fpr.boot.matrix, steps)
  FPR.VEC = round(1 - seq(0, 1, by = (1 / steps),3))
  colnames(rel.matrix) <- paste("TPR.AT.FPR.", FPR.VEC, sep = "")
  return(rel.matrix)
}

#' Calculates ROC curve thresholds
#' 
#' \code{calculate.thresholds} calculates the thresholds of the ROC curve
#' at which the curve changes directions. 
#' 
#' @param pred A numeric vector. Contains predictions. \code{calculate.thresholds} 
#'   assumes that a high prediction is evidence for the observation belonging 
#'   to the positive class.
#' @param true.class A logical vector. TRUE indicates the sample belonging to the
#'   positive class.
#' @return A numeric vector containing the thresholds. The length of the vector
#'   depends on the data. The number of thresholds tends to go down as the
#'   performance of the clasifier improves.   
#' 
#' @examples
#' x <- 1:10
#' y <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
#' calculate.thresholds(x, y) # relevant thresholds are 1, 4, 5, 7, 11
#' 
#' @export
calculate.thresholds <- function(pred, true.class) {
  pred <- c(pred, max(pred) + 1) # add one threshold since we use >
  index <- order(pred)
  pred <- pred[index]
  true.class <- true.class[index]
  is.threshold <- find_thresholds(pred, true.class) # use C++ to find thresholds
  thresholds <- pred[as.logical(is.threshold)]
  return(thresholds)
}