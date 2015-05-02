#' Calculate performance for bootstrapped ROC curve
#'
#' Calculates different performance metric for ROC curves based on the bootstrap
#' results saved in an object of class \code{fbroc.roc}. Confidence intervals
#' are included.
#'
#' @param roc An object of class \code{fbroc.roc}
#' @param metric A performance metric. Currently only "auc" is supported
#' @param conf.level The confidence level of the confidence interval
#' @return A list of class \code{fbroc.perf}, containing the elements:
#' \item{Observed.Performance}{The observed performance}
#' \item{CI.Performance}{Quantile based confidence interval for the performance}
#' \item{conf.level}{Confidence level of the confidence interval}
#' \item{metric}{Used performance metric}
#' \item{n.boot}{Number of bootstrap replicates used}
#' \item{boot.results}{Performance in each bootstrap replicate}
#' @seealso \code{\link{boot.roc}}, \code{\link{print.fbroc.perf}}, 
#'   \code{\link{plot.fbroc.perf}}
#' @examples
#' y <- rep(c(TRUE, FALSE), each = 500)
#' x <- rnorm(1000) + y
#' result.boot <- boot.roc(x, y, n.boot = 100)
#' perf.roc(result.boot, "auc")
#' perf.roc(result.boot, "auc", conf.level = 0.99)
#' @export
perf.roc <- function(roc, metric = "auc", conf.level = 0.95) {
  # start with data validation
  if (!is(roc, "fbroc.roc"))
    stop("roc must be of class fbroc.roc")
  if (length(metric) != 1 | class(metric) != "character")
    stop("metric must be character of length 1")
  if (!(metric %in% c("auc")))
    stop(paste(metric,"is not a valid performance metric"))
  # transform metric into number
  metric.number <- 0
  # call C++ to calculate actual results
  observed.perf <- get_roc_perf(matrix(roc$tpr.fpr.raw, nrow = 1), 
                                as.integer(metric.number))
  perf.boot <- get_roc_perf(roc$tpr.fpr.boot.matrix, as.integer(metric.number))
  
  # Quantile based confidence interval
  alpha <- 1 - conf.level
  alpha.levels <- c(alpha, 1 - alpha) 
  ci <- as.numeric(quantile(perf.boot, alpha.levels))
  names(ci) <- NULL
  
  
  perf <- list(Observed.Performance = observed.perf,
               CI.Performance = ci,
               conf.level = conf.level,
               metric = metric,
               n.boot = roc$n.boot,
               boot.results = perf.boot
               )
  class(perf) <- append(class(perf), "fbroc.perf")
  return(perf)
}