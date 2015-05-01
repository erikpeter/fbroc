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

#' @export
plot.fbroc.roc <- function(roc, col = "blue", fill = "cyan",
                           show.conf = TRUE, 
                           conf.level = 0.95) {
  plot.frame = roc$tpr.fpr
  
  roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
    ggtitle("ROC Curve") + xlab("False Positive Rate") +
    ylab("True Positive Rate") + theme_bw() +
    theme(title = element_text(size = 22),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  
  if (show.conf) {
    conf.frame <- conf.roc(roc, conf.level = conf.level, steps = 2000)
    roc.plot <- roc.plot + 
      geom_ribbon(data = conf.frame, fill = fill, alpha = 0.9,
                  aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
  }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col)
  print(roc.plot)
  invisible(roc.plot)
}
