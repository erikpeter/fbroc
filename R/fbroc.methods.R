#' Prints information about a \code{fbroc.perf} object
#' 
#' Prints the information about the bootstrap results for an object of class
#' \code{fbroc.perf}. This information includes the number of bootstrap
#' replicates, the metric used and the estimate with confidence interval.
#' @param x Object of class \code{fbroc.perf}.
#' @param ... further arguments passed to or from other methods.
#' @return Character containing the text that is also printed.
#' @seealso \code{\link{perf.roc}}
#' @export
print.fbroc.perf <- function(x, ...) {
  conf.level <- round(100 * x$conf.level, 0)
  text <- paste("\n", "
                Bootstrapped ROC performance metric", "\n", "\n",
                "Metric: ", x$metric, "\n",
                "Bootstrap replicates: ", x$n.boot, "\n", 
                "Estimate: ", round(x$Observed.Performance, 2), "\n",
                "Std. Error: ", round(sd(x$boot.results), 3), "\n", 
                conf.level,"% confidence interval:", "\n",
                round(x$CI.Performance[1], 2)," ",
                round(x$CI.Performance[2], 2), "\n", "\n",
                sep = "")
  cat(text)
  invisible(text)
}

#' Prints information about a \code{fbroc.roc} object
#' 
#' Prints the information about the bootstrap results for an object of class
#' \code{fbroc.roc}. This information includes the number of bootstrap
#' replicates, the time spent on bootstrapping, the AUC and the memory
#' usage of the object.
#' @param x Object of class \code{fbroc.roc}.
#' @param ... further arguments passed to or from other methods.
#' @return Character containing the text that is also printed.
#' @seealso \code{\link{boot.roc}}
#' @export
print.fbroc.roc <- function(x, ...) {
  x.mem <- round(as.numeric(object.size(x))/(1024*1024),0)
  adj <- ifelse(x$use.cache, "cached", "uncached")
  time <- ifelse(x$use.cache, "have been", "will be")
  text <- cat(paste("\n",
              "Bootstraped ",adj," ROC Curve with ", x$n.pos, " positive and ", x$n.neg,
            " negative samples. \n \n", "The AUC is ", round(x$auc, 2),".\n \n", 
            x$n.boot, " bootstrap samples ",time," calculated. \n",  "The results use up ", x.mem, 
            " MB of memory.", "\n", "\n", sep = ""))
  cat(text)
  invisible(text)
}

#' Plots a \code{fbroc.roc} object
#' 
#' Plot a \code{fbroc.roc} object and shows the ROC curve. The confidence
#' region for the ROC curve and the result for a specified performance metric 
#' can also be included in the plot. 
#' @param x Object of class \code{fbroc.roc}.
#' @param col Color used for the curve. Defaults to blue.
#' @param fill Color used for the confidence region. Defaults to royalblue1.
#' @param print.plot Logical specifying whether the plot should be printed.
#' @param show.conf Logical specifying whether the confidence region should be
#' plotted.
#' @param conf.level Confidence level of the confidence region.
#' @param show.metric Character specifying which metric to display. See 
#' \code{\link{perf.roc}} for details. Defaults to \code{NULL}, which means
#' that no metric is displayed.
#' @param ... further arguments passed to \code{\link{perf.roc}}.
#' @return A ggplot, so that the user can customize the plot further.
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y, n.boot = 100)
#' plot(result.boot)
#' @seealso \code{\link{boot.roc}}, \code{\link{perf.roc}}
#' @export
plot.fbroc.roc <- function(x, col = "blue", fill = "royalblue1", print.plot = TRUE,
                           show.conf = TRUE, conf.level = 0.95, 
                           show.metric = NULL, ...) {
  plot.frame = x$roc
  
  roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
    ggtitle("ROC Curve") + xlab("False Positive Rate") +
    ylab("True Positive Rate") + theme_bw() +
    theme(title = element_text(size = 22),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  
  if (show.conf) {
    conf.frame <- conf.roc(x, conf.level = conf.level)
    roc.plot <- roc.plot + 
      geom_ribbon(data = conf.frame, fill = fill, alpha = 0.5,
                  aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
  }
  if (!is.null(show.metric)) {
    perf <- perf.roc(x, metric = show.metric, conf.level = conf.level, ...)
    perf.text <- paste(perf$metric ," = " , round(perf$Observed.Performance, 2)," [",
                       round(perf$CI.Performance[1], 2), ",",
                       round(perf$CI.Performance[2], 2), "]", sep = "")
    if (show.metric == "tpr") {
      extra.frame <- data.frame(FPR = perf$params, TPR = perf$Observed.Performance,
                                lower = perf$CI.Performance[1], upper = perf$CI.Performance[2])
      roc.plot <- roc.plot + geom_errorbar(data = extra.frame, width = 0.02, size = 1.25,
                                             aes(ymin = lower, ymax = upper)) + 
                                             geom_point(data = extra.frame, size = 4)
    }
    if (show.metric == "fpr") {
      extra.frame <- data.frame(TPR = perf$params, FPR = perf$Observed.Performance,
                                lower = perf$CI.Performance[1], upper = perf$CI.Performance[2])
      roc.plot <- roc.plot + geom_errorbarh(data = extra.frame, height = 0.02, size = 1.25,
                                           aes(xmin = lower, xmax = upper)) +
                                           geom_point(data = extra.frame, size = 4)
    }
    text.frame <- data.frame(text.c = perf.text, TPR = 0.5, FPR = 0.68)
    roc.plot <- roc.plot + geom_text(size = 8, aes(label = text.c), data = text.frame)
    
  }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col)
  if (print.plot) print(roc.plot)
  invisible(roc.plot)
}

#' Plots ROC based performance metric as histogram
#' 
#' Given an object of class \code{perf.roc} this function plots the results of
#' the bootstrap as an histogram. The confidence interval is also included by
#' default.
#' 
#' @param x Object of class \code{perf.roc} to be plotted.
#' @param bins Number of bins for histogram. Default value depends on the number of bootstrap
#' values and the number of unique bootstrap performance values. 
#' @param col Color of outline of histogram bars. Defaults to white.
#' @param fill Fill of histogram bars. Defaults to lightblue.
#' @param print.plot Logical specifying whether the plot should be printed.
#' @param show.conf Logical specifying whether the confidence interval
#' should be displayed.
#' @param conf.text Logical specifying whether the confidence interval limits
#' should also be displayed as text.
#' @param ... Further arguments passed to or from other methods.
#' @return A ggplot, so that the user can customize the plot further.
#' @seealso \code{\link{perf.roc}}
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y, n.boot = 10000)
#' result.perf <- perf.roc(result.boot, "auc")
#' plot(result.perf)
#' @export
plot.fbroc.perf <- function(x, bins = NULL, col = "white", 
                            fill = "lightblue", print.plot = TRUE, 
                            show.conf = TRUE, conf.text = TRUE, ...) {
  boot.frame <- data.frame(x$boot.results)
  names(boot.frame) <- "Metric"
  if (is.null(bins)) {
    bins <- floor(x$n.boot/200)
    bins <- max(bins, 20)
    unique.val <- length(unique(boot.frame$Metric))
    bins <- min(bins, 60) 
    bins <- round(min(bins, unique.val / 4), 0)
    print(bins)
  }
  bw = round(diff(range(x$boot.results))/bins, 6)
  perf.plot <- ggplot(data = boot.frame, aes(x = Metric)) + 
               xlab(toupper(x$metric)) + ylab("Density") + 
               ggtitle("Performance histogram") +
               geom_histogram(fill = fill, col = col, aes(, y = ..density..), 
                              binwidth = bw) + theme_bw() +
               theme(title = element_text(size = 22),
                     axis.title.x = element_text(size = 18),
                     axis.title.y = element_text(size = 18),
                     axis.text.x = element_text(size = 16),
                     axis.text.y = element_text(size = 16))
  if (show.conf) {
    conf.frame <- data.frame(Metric = x$CI.Performance, y.dummy = 0)
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
