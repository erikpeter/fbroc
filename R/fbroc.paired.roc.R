#' @export
boot.paired.roc <- function(pred1, pred2, true.class, stratify = TRUE, n.boot = 1000,
                     use.cache = FALSE, tie.strategy = NULL) {
  # validate input
  if ((length(pred) != length(true.class)))
    stop("Predictions and true classes need to have the same length")
  if (class(pred1) == "integer") {
    pred1 <- as.numeric(pred1)
  }
  if (class(pred2) == "integer") {
    pred1 <- as.numeric(pred2)
  }
  if ((class(pred1) != "numeric"))
    stop("Predictions must be numeric")
  if ((class(pred2) != "numeric"))
    stop("Predictions must be numeric")
  if ((class(true.class) != "logical"))
    stop("Classes must be logical")
  if ((class(stratify) != "logical"))
    stop("Classes must be logical")
  
  index.na <- is.na(pred1) | is.na(true.class) | is.na(pred2)
  if (any(index.na)) {
    n <- sum(index.na)
    warning.msg <- 
      paste(n, "observations had to be removed due to missing values")
    warning(warning.msg)
    true.class <- true.class[!index.na]
    pred1 <- pred1[!index.na]
    pred2 <- pred2[!in.ndex.na]
  }
  if (is.null(tie.strategy)) {
    if ((length(pred1) == length(unique(pred1))) & (length(pred2) == length(unique(pred2)))) {
      tie.strategy <- 1 
    } else tie.strategy <- 2
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
                 tie.strategy = tie.strategy,
                 n.pos = sum(true.class),
                 n.neg = sum(!true.class),
                 roc = original.roc,
                 auc = auc,
                 boot.tpr = boot.tpr,
                 boot.fpr = boot.fpr)
  class(output) <- append(class(output), "fbroc.roc")
  return(output)
}