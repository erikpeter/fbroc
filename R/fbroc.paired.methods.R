#' @export
extract.single.roc <- function(x, index) {
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
    output$roc = output$roc1
    output$boot.tpr = x$boot.tpr1
    output$boot.fpr = x$boot.fpr1
    output$n.thresholds <- x$n.thresholds1
  } else {
    output$auc <- x$auc2
    output$predictions = x$predictions2
    output$roc = output$roc2
    output$boot.tpr = x$boot.tpr2
    output$boot.fpr = x$boot.fpr2
    output$n.thresholds <- x$n.thresholds2
  }
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
                                  col = "blue", 
                                  fill = "royalblue1", 
                                  col2 = "red",
                                  fill2 = "orangered2",
                                  print.plot = TRUE,
                                  show.conf = TRUE, conf.level = 0.95, 
                                  show.metric = NULL, ...) {

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

  roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
    ggtitle("ROC Curve") + xlab("False Positive Rate") +
    ylab("True Positive Rate") + theme_bw() +
    theme(title = element_text(size = 22),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  
   if (show.conf) {
     conf.frame <- conf.roc(extract.single.roc(x, 1), conf.level = conf.level)
     conf.frame$Segment <- 1
     roc.plot <- roc.plot + 
       geom_ribbon(data = conf.frame, fill = fill, alpha = 0.5,
                   aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
     conf.frame2 <- conf.roc(extract.single.roc(x, 2), conf.level = conf.level)
     conf.frame2$Segment <- 1
     roc.plot <- roc.plot + 
       geom_ribbon(data = conf.frame2, fill = fill2, alpha = 0.5,
                   aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
   }
#   if (!is.null(show.metric)) {
#     perf <- perf.roc(x, metric = show.metric, conf.level = conf.level, ...)
#     perf.text <- paste(perf$metric ," = " , round(perf$Observed.Performance, 2)," [",
#                        round(perf$CI.Performance[1], 2), ",",
#                        round(perf$CI.Performance[2], 2), "]", sep = "")
#     if (show.metric == "tpr") {
#       extra.frame <- data.frame(FPR = perf$params, TPR = perf$Observed.Performance, Segment = 1,
#                                 lower = perf$CI.Performance[1], upper = perf$CI.Performance[2])
#       roc.plot <- roc.plot + geom_errorbar(data = extra.frame, width = 0.02, size = 1.25,
#                                            aes(ymin = lower, ymax = upper)) + 
#         geom_point(data = extra.frame, size = 4)
#     }
#     if (show.metric == "fpr") {
#       extra.frame <- data.frame(TPR = perf$params, FPR = perf$Observed.Performance, Segment = 1,
#                                 lower = perf$CI.Performance[1], upper = perf$CI.Performance[2])
#       roc.plot <- roc.plot + geom_errorbarh(data = extra.frame, height = 0.02, size = 1.25,
#                                             aes(xmin = lower, xmax = upper)) +
#         geom_point(data = extra.frame, size = 4)
#     }
#     text.frame <- data.frame(text.c = perf.text, TPR = 0.5, FPR = 0.68, Segment = 1)
#     roc.plot <- roc.plot + geom_text(size = 8, aes(label = text.c), data = text.frame)
#     
#   }
  roc.plot <- roc.plot + geom_path(size = 1.1, col = col)
  roc.plot <- roc.plot + geom_path(data = plot.frame2, col = col2, size = 1.1)
  if (print.plot) print(roc.plot)
  invisible(roc.plot)
}