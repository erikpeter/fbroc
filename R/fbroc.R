#' fbroc: A package for fast bootstrap analysis of ROC curves
#' 
#' Fbroc enables the fast bootstrap analysis of ROC curves for simulation
#' studies and shiny applications by using a fast
#' algorithm where the cost of a single bootstrap replicate is \eqn{O(n)}, with 
#' n denoting the number of observations. The algorithm is implemented in C++ to further
#' increase the efficiency. Currently 100000 bootstrap iterations for 500
#' observations take about one second. The ROC curve as used shows
#' the True Positive Rate (TPR) as a function of the False Positive Rate (FPR).
#' 
#' @section Important fbroc functions:
#' \describe{
#' \item{\code{\link{boot.roc}}}{Use \code{boot.roc} to bootstrap a ROC curve.}
#' \item{\code{\link{conf.roc}}}{Calculate a confidence region for the ROC curve.}
#' \item{\code{\link{perf.roc}}}{Estimate performance and calculate confidence
#' intervals.}
#' \item{\code{\link{boot.tpr.at.fpr}}}{Transforms bootstrapped TPR/FPR at 
#' threshold matrix into a matrix containing bootstrapped TPRs at a range of
#' different FPRs.}
#' }
#' @section Example Data:
#' fbroc also contains the example data set \link{roc.examples}, 
#' which you can use to test the functionality of the
#' package. This data set contains simulated data and not an real application.
#' @section Details:
#' The algorithm works by first determining the critical thresholds of the ROC
#' curve - cutoffs at which the curve changes directions. Each observation is then linked
#' to the specific thresholds at which they first cause a change in the TPR
#' or FPR. Calculating this link and directly bootstrapping that link
#' allows us to construct the bootstrapped ROC
#' curve very quickly. Since multiple observation can be linked to the same
#' threshold, it is difficult to implement the algorithm efficiently in R. 
#' This is why \code{fbroc} implements it in C++.
#' \cr \cr
#' All bootstrap confidence interval are based on the percentile method.
#' @section Notes:
#' Package \code{fbroc} is still in an early development stage. Currently it supports bootstrapping
#' the confidence region of the ROC curve, as well as the AUC, the FPR at a fixed TPR and vice versa.
#' In addition, support for paired ROC curves and more sophisticated bootstrap confidence interval 
#' calculation will be added at a later time.
#' @examples
#' data(roc.examples)
#' result.boot <- boot.roc(roc.examples$Cont.Pred, roc.examples$True.Class, n.boot = 100)
#' plot(result.boot)
#' perf.roc(result.boot, "auc")
#' perf.roc(result.boot, "auc", conf.level = 0.99)
#' perf.roc(result.boot, "tpr", conf.level = 0.95, fpr = 0.1)
#' @references Efron, B., & Tibshirani, R. (1998). \emph{An introduction to the bootstrap.}
#' Boca Raton, Fla: Chapman & Hall/CRC. 
#' @useDynLib fbroc
#' @import ggplot2
#' @importFrom Rcpp sourceCpp
#' @docType package
#' @name fbroc
NULL


.onUnload <- function(libpath) {
  library.dynam.unload("fbroc", libpath)
}


# hack recommend by Hadley Wickham to make CMD check issue no notes
globalVariables(c("TPR", "FPR", "text.c", "Lower.TPR", "Upper.TPR", "lower", "upper",
                  "Metric", "y.dummy", "..density..", "Segment"))


