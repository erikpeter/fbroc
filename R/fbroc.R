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

#' Bootstrap ROC curve
#'
#' \code{boot.roc} calculates the ROC curve, initialises the settings for the
#' bootstrap and calculates the bootstrap results for the true and false
#' positive rate at every relevant threshold. Missing values are removed with 
#' a warning prior to bootstrapping.
#'
#' @param pred A numeric vector. Contains predictions. \code{boot.roc} assumes
#'   that a high prediction is evidence for the observation belonging to the
#'   positive class.
#' @param true.class A logical vector. TRUE indicates the sample belonging to the
#'   positive class.
#' @param stratify Logical. Indicates whether we use stratified boostrap.
#'   Default to TRUE. Non-stratified bootstrap is not yet implemented.
#' @param n.boot A number that will be coerced to integer. Specified the 
#'   number of bootstrap replicates. Defaults to 1000.
#' @param seed A number that will be coerced to integer. Used to initialise the
#'   random number generator used. If not specified will be set to a random number
#'   between 1 and 1e7.
#' 
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y)
#' 
#' @export
boot.roc <- function(pred, true.class, stratify = TRUE, n.boot = 1000, seed = NULL) {
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
  
  if (is.null(seed)) seed <- runif(1, 1, 1e7)
  
  n.boot <- as.integer(n.boot)
  seed <- as.integer(seed)
  if (length(n.boot) != 1)
    stop("n.boot must have length 1")
  if (length(seed) != 1)
    stop("seed must have length 1")
  if (length(stratify) != 1)
    stop("stratify must have length 1")
  
  if (!stratify) stop("Non-stratified bootstrapping is not yet supported")
  
  thresholds <- calculate.thresholds(pred, true.class)
  n.thresholds <- length(thresholds)  
  
  bench <- system.time(tpr.fpr.boot <- 
                         tpr_fpr_boot(pred, as.integer(true.class),
                                      thresholds, n.boot, seed))[1]
  tpr.fpr <- true_tpr_fpr(pred, as.integer(true.class), thresholds)
  
  auc <- get_auc(matrix(tpr.fpr, nrow = 1))
  
  output <- list(predictions = pred,
                 true.classes = true.class,
                 thresholds = thresholds,
                 n.thresholds = n.thresholds,
                 seed = seed,
                 n.boot = n.boot,
                 n.pos = sum(true.class),
                 n.neg = sum(!true.class),
                 tpr.fpr = tpr.fpr,
                 time.used = bench,
                 auc = auc,
                 tpr.fpr.boot.matrix = tpr.fpr.boot)  
  class(output) <- append(class(output), "fbroc.roc")
  return(output)
}

#' @export
print.fbroc.roc <- function(roc) {
  mem.roc <- round(as.numeric(object.size(roc)/(1024*1024)), 1) 
  time = "not yet calculated"
  cat(paste("Bootstraped ROC Curve with ", roc$n.pos, " positive and ", roc$n.neg,
            " negative samples. \n \n", "The AUC is ", roc$auc,".\n \n", 
            roc$n.boot, " bootstrap samples have been calculated with seed ", 
            roc$seed, ". \n", "The calculation took ", roc$time.used, 
            " seconds and the results use up ", mem.roc, " MB of memory.", sep = ""))
  invisible(NULL)
}

