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
                                  fill = "blue1", 
                                  col2 = "red",
                                  fill2 = "red1",
                                  print.plot = TRUE,
                                  show.conf = TRUE, conf.level = 0.95, 
                                  show.metric = NULL, 
                                  plots = 1:4,
                                  ...) {

  if (!all(plots %in% 1:4) | (length(plots) > 4)) stop("Invalid plots argument")
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
  if (1 %in% plots) {
    roc.plot <- ggplot(data = plot.frame, aes(x = FPR, y = TPR)) +               
      ggtitle("ROC Curve") + xlab("False Positive Rate") +
      ylab("True Positive Rate") + theme_bw() +
      theme(title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    roc1 <- extract.single.roc(x, 1)
    roc2 <- extract.single.roc(x, 2)
    if (show.conf) {
      conf.frame <- conf.roc(roc1, conf.level = conf.level)
      conf.frame$Segment <- 1
      roc.plot <- roc.plot + 
        geom_ribbon(data = conf.frame, fill = fill, alpha = 0.5,
                    aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
      conf.frame2 <- conf.roc(roc2, conf.level = conf.level)
      conf.frame2$Segment <- 1
      roc.plot <- roc.plot + 
        geom_ribbon(data = conf.frame2, fill = fill2, alpha = 0.5,
                    aes(y = NULL, ymin = Lower.TPR, ymax = Upper.TPR))
    }
    if (!is.null(show.metric)) {
      perf <- perf.paired.roc(x, metric = show.metric, conf.level = conf.level, ...)
      #perf <- perf.roc(roc1, metric = show.metric, conf.level = conf.level, ...)
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
      if (show.metric == "tpr") {
        extra.frame <- data.frame(FPR = perf$params, 
                                  TPR = c(perf$Observed.Performance.Predictor1, 
                                          perf$Observed.Performance.Predictor2),
                                  Segment = 1,
                                  lower = c(perf$CI.Performance.Predictor1[1], 
                                            perf$CI.Performance.Predictor2[1]),
                                  upper = c(perf$CI.Performance.Predictor1[2],
                                            perf$CI.Performance.Predictor2[2]))
        roc.plot <- roc.plot + geom_errorbar(data = extra.frame, width = 0.02, size = 1.25,
                                             aes(ymin = lower, ymax = upper)) + 
          geom_point(data = extra.frame, size = 4)
      }
      if (show.metric == "fpr") {
        extra.frame <- data.frame(TPR = perf$params, 
                                  FPR = c(perf$Observed.Performance.Predictor1, 
                                          perf$Observed.Performance.Predictor2),
                                  Segment = 1,
                                  lower = c(perf$CI.Performance.Predictor1[1], 
                                            perf$CI.Performance.Predictor2[1]),
                                  upper = c(perf$CI.Performance.Predictor1[2],
                                            perf$CI.Performance.Predictor2[2]))
        roc.plot <- roc.plot + geom_errorbarh(data = extra.frame, height = 0.02, size = 1.25,
                                              aes(xmin = lower, xmax = upper)) +
          geom_point(data = extra.frame, size = 4)
      }
      perf.text.vector <- paste(perf.text, perf.text2, perf.text3, sep ="\n")
      text.frame <- data.frame(text.c = perf.text.vector, 
                               TPR = 0.55, 
                               FPR = 0.68, 
                               Segment = 1)
      roc.plot <- roc.plot + geom_text(size = 8, aes(label = text.c), data = text.frame)
      #     
    }
    roc.plot <- roc.plot + geom_path(size = 1.1, col = col)
    roc.plot <- roc.plot + geom_path(data = plot.frame2, col = col2, size = 1.1)
    if (print.plot) print(roc.plot)
  }
  if (2 %in% plots) {
    tpr1 <- tpr_at_fpr_cached(matrix(x$roc1$TPR, nrow = 1), 
                              matrix(x$roc1$FPR, nrow = 1),
                              x$n.thresholds1, 
                              100)
    tpr2 <- tpr_at_fpr_cached(matrix(x$roc2$TPR, nrow = 1), 
                              matrix(x$roc2$FPR, nrow = 1),
                              x$n.thresholds2, 
                              100)
    plot.frame <- data.frame(Delta.TPR = as.vector(tpr1 - tpr2), FPR = seq(1, 0, by = -0.01))
    roc.plot2 <- ggplot(data = plot.frame, aes(x = FPR, y = Delta.TPR))
    
    if (show.conf) {
      if (x$use.cache) {
        stop("Use of cache not yet implemented")
      } else {
        conf.frame <- 
          conf.roc.paired(x, conf.level = conf.level, steps = 100)
        
      }
      roc.plot2 <- roc.plot2 + 
        geom_ribbon(data = conf.frame, fill = "purple1", alpha = 0.5,
                    aes(y = NULL, ymin = Lower.Delta.TPR, ymax = Upper.Delta.TPR))
      
    }
    
  
    roc.plot2 <- roc.plot2 + geom_line(size = 1.1, col = "purple")
    
    roc.plot2 <- roc.plot2 +ggtitle("Differential TPR") + xlab("False Positive Rate") +
      ylab("Delta True Positive Rate") + theme_bw() +
      theme(title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    
    if (print.plot) print(roc.plot2)

  }
  
  if (3 %in% plots) {
    fpr1 <- fpr_at_tpr_cached(matrix(x$roc1$TPR, nrow = 1), 
                              matrix(x$roc1$FPR, nrow = 1),
                              x$n.thresholds1, 
                              100)
    fpr2 <- fpr_at_tpr_cached(matrix(x$roc2$TPR, nrow = 1), 
                              matrix(x$roc2$FPR, nrow = 1),
                              x$n.thresholds2, 
                              100)
    plot.frame <- data.frame(Delta.FPR = as.vector(fpr1 - fpr2), TPR = seq(1, 0, by = -0.01))
    roc.plot3 <- ggplot(data = plot.frame, aes(x = Delta.FPR, y = TPR))
    
    # insert confidence code here
    
    roc.plot3 <- roc.plot3 + geom_path(size = 1.1, col = "purple")
    
    roc.plot3 <- roc.plot3 +ggtitle("Differential FPR") + xlab("Delta False Positive Rate") +
      ylab("True Positive Rate") + theme_bw() +
      theme(title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16))
    
    if (print.plot) print(roc.plot3)
  }
  
  
  invisible(roc.plot)
}