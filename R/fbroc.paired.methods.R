#' Prints information about a \code{fbroc.perf.paired} object
#' 
#' Prints the information about the bootstrap results for an object of class
#' \code{fbroc.perf.paired}. This information includes the number of bootstrap
#' replicates, the metric used and estimates for both the individual classifiers and the
#' difference in performance including confidence intervals.
#' @param x Object of class \code{fbroc.perf}.
#' @param ... further arguments passed to or from other methods.
#' @return Character containing the text that is also printed.
#' @seealso \code{\link{perf.paired.roc}}
#' @export
print.fbroc.perf.paired <- function(x, ...) {
  conf.level <- round(100 * x$conf.level, 0)
  text <- paste("\n", "
                Bootstrapped ROC performance metric", "\n", "\n",
                "Metric: ", x$metric, "\n",
                "Bootstrap replicates: ", x$n.boot, "\n", "\n",
                "Classifier 1: ", "\n",
                "Observed:", round(x$Observed.Performance.Predictor1, 3), "\n",
                "Std. Error: ", round(sd(x$boot.results.pred1), 3), "\n", 
                conf.level,"% confidence interval:", "\n",
                round(x$CI.Performance.Predictor1[1], 3)," ",
                round(x$CI.Performance.Predictor1[2], 3), "\n", "\n", 
                "Classifier 2: ", "\n",
                "Observed:", round(x$Observed.Performance.Predictor2, 3), "\n",
                "Std. Error: ", round(sd(x$boot.results.pred2), 3), "\n", 
                conf.level,"% confidence interval:", "\n",
                round(x$CI.Performance.Predictor2[1], 3)," ",
                round(x$CI.Performance.Predictor2[2], 3), "\n", "\n", 
                "Delta: ", "\n",
                "Observed:", round(x$Observed.Difference, 3), "\n",
                "Std. Error: ", round(sd(x$boot.results.pred1 - x$boot.results.pred2), 3), "\n", 
                conf.level,"% confidence interval:", "\n",
                round(x$CI.Performance.Difference[1], 3)," ",
                round(x$CI.Performance.Difference[2], 3), "\n", "\n",
                "Correlation: ", round(x$Cor, 2), "\n", "\n",
                sep = "")
  cat(text)
  invisible(text)
}



#' Plots ROC based performance metric as histogram
#' 
#' PLACEHOLDER
#' 
#' @inheritParams plot.fbroc.perf
#' @return A ggplot, so that the user can customize the plot further.
#' @seealso \code{\link{perf.paired.roc}}
#' @export
plot.fbroc.perf.paired <- function(x, bins = NULL, col = "white", 
                            fill = "lightblue", print.plot = TRUE, 
                            show.conf = TRUE, conf.text = TRUE, ...) {
  boot.frame <- data.frame(x$boot.results.pred1 - x$boot.results.pred2)
  names(boot.frame) <- "Metric"
  if (is.null(bins)) {
    # Bin number heuristic
    bins <- floor(x$n.boot/200)
    bins <- max(bins, 20)
    bins <- min(bins, 60)
    bw.min <- 0.99999*min(diff(sort(unique(boot.frame$Metric))))
    bw = round(diff(range(boot.frame$Metric))/bins, 6)
    if ((bw < bw.min) | (5*bw.min > bw)) bw <- bw.min
  }
  else bw = round(diff(range(x$boot.results))/bins, 6)
  
  perf.plot <- ggplot(data = boot.frame, aes(x = Metric)) + 
    xlab(substitute(paste(Delta, a), list(a = toupper(x$metric)))) + ylab("Density") + 
    ggtitle("Performance histogram") +
    geom_histogram(fill = fill, col = col, aes(, y = ..density..), 
                   binwidth = bw) + theme_bw() +
    theme(title = element_text(size = 22),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  if (show.conf) {
    conf.frame <- data.frame(Metric = x$CI.Performance.Difference, y.dummy = 0)
    perf.plot <- perf.plot + geom_line(data = conf.frame, aes(y=y.dummy), 
                                       col = "black",
                                       size = 2)
    if (conf.text) {
      conf.frame$text.c <- round(conf.frame$Metric,2)
      perf.plot <- perf.plot + 
        geom_text(data = conf.frame,
                  aes(x = Metric, y=y.dummy, label = text.c), 
                  vjust = 0, hjust = c(1,0), size = 8)
    }
  }
  if (print.plot) print(perf.plot)
  invisible(perf.plot)
}


#' @export
extract.roc <- function(x, index) {
  if (index != 1 & index != 2) stop("Index must be 1 or 2")
  output <- vector("list", 12)
  names(output) <- c("predictions", "true.classes", "n.thresholds", "n.boot", "use.cache",
                     "tie.strategy", "n.pos", "n.neg", "roc", "auc", "boot.tpr", "boot.fpr")
  output$true.classes <- x$true.classes
  output$n.boot <- x$n.boot
  output$n.pos <- x$n.pos
  output$n.neg <- x$n.neg
  output$use.cache = x$use.cache
  output$tie.strategy = x$tie.strategy

  if (index == 1) {
    output$auc <- x$auc1
    output$predictions = x$predictions1
    output$roc = x$roc1
    output$boot.tpr = x$boot.tpr1
    output$boot.fpr = x$boot.fpr1
    output$n.thresholds <- x$n.thresholds1
  } else {
    output$auc <- x$auc2
    output$predictions = x$predictions2
    output$roc = x$roc2
    output$boot.tpr = x$boot.tpr2
    output$boot.fpr = x$boot.fpr2
    output$n.thresholds <- x$n.thresholds2
  }
  class(output) <- append(class(output), "fbroc.roc")
  return(output)
}



#' Plots a \code{fbroc.paired.roc} object
#' 
#' PLACEHOLDER Plot a \code{fbroc.roc} object and shows the ROC curve. The confidence
#' region for the ROC curve and the result for a specified performance metric 
#' can also be included in the plot. 
#' 
#' @param ... further arguments passed to \code{\link{perf.roc}}.
#' @inheritParams plot.fbroc.roc
#' @return A ggplot, so that the user can customize the plot further.
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y, n.boot = 100)
#' plot(result.boot)
#' @seealso \code{\link{boot.roc}}, \code{\link{perf.roc}}
#' @export
plot.fbroc.paired.roc <- function(x, 
                                  col1 = "blue", 
                                  fill1 = "dodgerblue", 
                                  col2 = "darkgreen",
                                  fill2 = "seagreen1",
                                  print.plot = TRUE,
                                  show.conf = TRUE, 
                                  conf.level = 0.95, 
                                  steps = 250,
                                  show.metric = "auc", 
                                  ...) {
  if (x$tie.strategy == 2) {
    expand.roc <- add_roc_points(x$roc1$TPR, x$roc1$FPR)
    plot.frame <- data.frame(TPR = expand.roc[[1]],
                             FPR = expand.roc[[2]],
                             Segment = expand.roc[[3]])
    expand.roc <- add_roc_points(x$roc2$TPR, x$roc2$FPR)
    plot.frame2 <- data.frame(TPR = expand.roc[[1]],
                               FPR = expand.roc[[2]],
                               Segment = expand.roc[[3]])
  } else {
    plot.frame = x$roc1
    plot.frame$Segment = 1
    plot.frame2 = x$roc2
    plot.frame2$Segment = 1
  }
  
  roc.plot <- fbroc.plot.base(plot.frame)
  
  roc1 <- extract.roc(x, 1)
  roc2 <- extract.roc(x, 2)
  if (show.conf) {
    roc.plot <- roc.plot + 
                fbroc.plot.add.conf(roc1, conf.level = conf.level, steps = steps, fill = fill1)
    roc.plot <- roc.plot +
                fbroc.plot.add.conf(roc2, conf.level = conf.level, steps = steps, fill = fill2)
  }
  if (!is.null(show.metric)) {
    perf <- perf(x, metric = show.metric, conf.level = conf.level, ...)
    perf.text <- paste("Predictor 1 ", perf$metric ," = " , 
                       round(perf$Observed.Performance.Predictor1, 2)," [",
                       round(perf$CI.Performance.Predictor1[1], 2), ",",
                       round(perf$CI.Performance.Predictor1[2], 2), "]", sep = "")
    perf.text2 <- paste("Predictor 2 ",perf$metric ," = " , 
                        round(perf$Observed.Performance.Predictor2, 2)," [",
                        round(perf$CI.Performance.Predictor2[1], 2), ",",
                        round(perf$CI.Performance.Predictor2[2], 2), "]", sep = "")
    perf.text3 <- paste("Delta ",perf$metric ," = " , 
                        round(perf$Observed.Difference, 2)," [",
                        round(perf$CI.Performance.Difference[1], 2), ",",
                        round(perf$CI.Performance.Difference[2], 2), "]", sep = "")
    
    roc.plot <- fbroc.plot.add.metric.paired(roc.plot, show.metric, perf, col1, col2)
    perf.text.vector <- paste(perf.text, perf.text2, perf.text3, sep ="\n")
    text.frame <- data.frame(text.c = perf.text.vector, 
                             TPR = 0.55, 
                             FPR = 0.4, 
                             Segment = 1)
    
    roc.plot <- roc.plot + geom_text(size = 8, aes(label = text.c), data = text.frame, hjust = 0)
    #     
  }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col1)
  roc.plot <- roc.plot + geom_path(data = plot.frame2, col = col2, size = 1.1)
  if (print.plot) print(roc.plot)
  
  invisible(roc.plot)
}

#' @export
plot.fbroc.conf.paired <- function(x, col = "blue", fill = "royalblue1", print.plot = TRUE,...) {
  if (names(x)[1] == "FPR") { # tpr over fpr
    roc.plot <- ggplot(data = x, aes(x = FPR, y = Delta.TPR)) +               
      ggtitle("ROC Curve") + ylab(substitute(paste(Delta, a), list(a = "True Positive Rate"))) +
      xlab("False Positive Rate") + theme_bw() +
      theme(title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    #plot conf
    roc.plot <- roc.plot + geom_ribbon(data = x, fill = fill, alpha = 0.5,
                                       aes(y = NULL, ymin = Lower.Delta.TPR, ymax = Upper.Delta.TPR))
  }
  else { # Now the same plot for curve fpr over tpr
    roc.plot <- ggplot(data = x, aes(y = Delta.FPR, x = TPR)) +               
      ggtitle("ROC Curve") +  ylab(substitute(paste(Delta, a), list(a = "False Positive Rate"))) +
      xlab("True Positive Rate") + theme_bw() +
      theme(title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    #plot conf
    roc.plot <- roc.plot + geom_ribbon(data = x, fill = fill, alpha = 0.5,
                                       aes(y = NULL, ymin = Lower.Delta.FPR, ymax = Upper.Delta.FPR))
  }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col) # plot estimate
  
  if (print.plot) print(roc.plot)
  invisible(roc.plot)
}

