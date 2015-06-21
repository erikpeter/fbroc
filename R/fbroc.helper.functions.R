c.list.to.roc <- function(input) {
  roc.frame <- as.data.frame(input[1:3])
  names(roc.frame) <- c("TPR", "FPR", "threshold")
  return(roc.frame)
}

validate.single.numeric <- function(number, var.name) {
  if (is.null(number)) stop(paste("Please pass ", var.name, " to perf.roc!", sep = ""))
  if (class(number) != "numeric") stop(paste(var.name, " must be numeric!", sep = ""))
  if (length(number) != 1) stop(paste(var.name, " must have length 1!", sep = ""))
  if (is.na(number)) stop(paste(var.name, " is NA!", sep = ""))
  if ((number < 0) | (number > 1)) stop(paste(var.name, " must be in [0, 1]!", sep = ""))
  return(number)
}
