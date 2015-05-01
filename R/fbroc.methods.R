#' @export
print.fbroc.roc <- function(x, ...) {
  mem.x <- round(as.numeric(object.size(x)/(1024*1024)), 1) 
  time = "not yet calculated"
  cat(paste("Bootstraped ROC Curve with ", x$n.pos, " positive and ", x$n.neg,
            " negative samples. \n \n", "The AUC is ", x$auc,".\n \n", 
            x$n.boot, " bootstrap samples have been calculated. \n", "The calculation took ", x$time.used, 
            " seconds and the results use up ", mem.x, " MB of memory.", sep = ""))
  invisible(NULL)
}

#' @export
plot.fbroc.roc <- function(x, col = "blue", fill = "cyan",
                           show.conf = TRUE, conf.level = 0.95, 
                           show.metric = NULL, ...) {
  plot.frame = x$tpr.fpr
  
  roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
    ggtitle("ROC Curve") + xlab("False Positive Rate") +
    ylab("True Positive Rate") + theme_bw() +
    theme(title = element_text(size = 22),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  
  if (show.conf) {
    conf.frame <- conf.roc(x, conf.level = conf.level, steps = 2000)
    roc.plot <- roc.plot + 
      geom_ribbon(data = conf.frame, fill = fill, alpha = 0.9,
                  aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
  }
  if (!is.null(show.metric)) {
    perf <- perf.roc(x, metric = show.metric, conf.level = conf.level)
    perf.text <- paste("AUC = ", round(perf$Observed.Performance, 2)," [",
                       round(perf$CI.Performance[1], 2), ",",
                       round(perf$CI.Performance[2], 2), "]", sep = "")
    if (show.metric == "auc") {
      text.frame <- data.frame(text = perf.text, TPR = 0.5, FPR = 0.68)
      roc.plot <- roc.plot + geom_text(size = 8, aes(label = text), data = text.frame)
    }
  }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col)
  print(roc.plot)
  invisible(roc.plot)
}
