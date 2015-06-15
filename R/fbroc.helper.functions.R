c.list.to.roc <- function(input) {
  roc.frame <- as.data.frame(input[1:3])
  names(roc.frame) <- c("TPR", "FPR", "threshold")
  return(roc.frame)
}
