#' @useDynLib fbroc
#' @importFrom Rcpp sourceCpp
NULL


.onUnload <- function(libpath) {
  library.dynam.unload("fbroc", libpath)
}

calculate.thresholds <- function(pred, true.class) {
  thresholds <- sort(unique(pred)) # we do not need separate threshold for duplicate values
  n.thresholds <- length(thresholds)
  keep.in <- rep(TRUE, length(thresholds))
  # turn this into C++ later
  last.threshold <- thresholds[1]
  for (i in 2:n.thresholds) {
    classes.between <- true.class[(pred >= last.threshold ) & (pred <= thresholds[i])]
    if (all(classes.between) | all(!classes.between)) keep.in[i] <- FALSE
      else last.threshold <- thresholds[i]      
    }
  thresholds <- thresholds[keep.in]
  thresholds <- c(thresholds, max(pred) + 1)
  return(thresholds)
}

#' @export
boot.roc <- function(pred, true.class, n.boot = 1000, seed = 123) {
  thresholds <- calculate.thresholds(pred, true.class)
  n.thresholds <- length(thresholds)  
  tpr_fpr_boot(pred, as.integer(true.class), thresholds, n.boot, as.integer(seed))
}


# some example code
#' @export
test.fun <- function(n.boot = 1000) { 
  y <- rep(c(TRUE, FALSE), each = 500)
  x <- rnorm(1000) + y
  calculate.thresholds(x, y)
  boot.roc(x,y, n.boot = n.boot)
}

# some example code
#' @export
test.fun2 <- function(n.boot = 1000) { 
  y <- rep(c(TRUE, FALSE), each = 500)
  x <- rnorm(1000) + y
  calculate.thresholds(x, y)
  get_auc(boot.roc(x,y, n.boot = n.boot))
}

# some example code
#' @export
get.auc.only <- function(pred, true.class) {
  thresholds <- calculate.thresholds(pred, true.class)
  n.thresholds <- length(thresholds)  
  vec <- true_tpr_fpr(pred, as.integer(true.class), thresholds)
  auc <- get_auc(matrix(vec, nrow = 1))
  return(auc)
}

# some example code
#' @export
test.auc <- function() {
  y <- rep(c(TRUE, FALSE), each = 500)
  x <- rnorm(1000) + y
  auc.fbroc <- get.auc.only(x, y)
  require(ROCR)
  pred.obj <- prediction(x,y)
  auc.rocr <- performance(pred.obj, "auc")@y.values[[1]]
  print(auc.fbroc - auc.rocr)
}

# some example code
#' @export
test.boot.auc <- function(n, n.boot) {
  y <- rep(c(TRUE, FALSE), each = n)
  x <- rnorm(n) + y
  require(pROC)
  # check time
  print(system.time(get_auc(boot.roc(x,y, n.boot = n.boot))))
  
  
  
  aucs.fbroc <- get_auc(boot.roc(x,y, n.boot = n.boot))
  print(quantile(aucs.fbroc, c(0.025, 0.975)))
  print(system.time(ergebnis <- ci(roc(y~x), method = "bootstrap", progress = "none", boot.n = n.boot, algorithm = 3)))
  print(ergebnis)
}
