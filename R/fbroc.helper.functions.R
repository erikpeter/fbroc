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


fbroc.plot.base <- function(plot.frame) {
  roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
              ggtitle("ROC Curve") + xlab("False Positive Rate") +
              ylab("True Positive Rate") + theme_bw() +
              theme(title = element_text(size = 22),
                axis.title.x = element_text(size = 18),
                axis.title.y = element_text(size = 18),
                axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16))
  return(roc.plot)
}

fbroc.plot.add.conf <- function(roc1, conf.level = conf.level, steps = steps, fill = fill) {
  conf.frame <- conf(roc1, conf.level = conf.level, steps = steps)
  conf.frame$Segment <- 1
 
  geom_ribbon(data = conf.frame, fill = fill, alpha = 0.5,
              aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
}


