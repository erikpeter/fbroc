#' @useDynLib fbroc
#' @importFrom Rcpp sourceCpp
NULL

calculate.thresholds <- function(pred, true.class) {
  thresholds <- unique(pred) # we do not need separate threshold for duplicate values
  n.thresholds <- length(thresholds)
  keep.in <- rep(TRUE, length(thresholds))
  # turn this into C++ later
  last.threshold <- thresholds[1]
  for (i in 2:n.thresholds) {
    classes.between <- true.class[(pred >= last.threshold ) & (pred <= thresholds[i])]
    if (all(classes.between) | all(!classes.between)) keep.in[i] <- FALSE
      else last.threshold <- thresholds[i]      
    }
  thresholds <- sort(thresholds[keep.in])
  thresholds <- c(thresholds, max(pred) + 1)
  return(thresholds)
}

boot.roc <- function(pred, true.class, n.boot = 1000) {
  thresholds <- calculate.thresholds(pred, true.class)
  n.thresholds <- length(thresholds)  
  tpr_fpr(pred, as.integer(true.class), thresholds)
}


# some example code
test.fun <- function() {
  x <- 1:10
  y <- !c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  calculate.thresholds(x, y)
  boot.roc(x,y)
}