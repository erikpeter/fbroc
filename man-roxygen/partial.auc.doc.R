#' @section Note on partial AUC correction:
#' 
#' The partial AUC is hard to interpret without keeping in mind the range on which it is calculated.
#' For example, if the ROC Curve is integrated over the FPR interval [0, 0.1] a completely random
#' and non-discrimate classifier would have a partial AUC of only 0.05. The same classifier over
#' the interval [0.9, 1] would have a partial AUC of 0.95 even though both intervals are of the
#' same width.
#' 
#' The correction by McClish produces a correct partial AUC given by
#' \deqn{\frac{1}{2} \Big(1 + \frac{\textrm{partialAUC} - \textrm{auc.min}}{\textrm{auc.max} 
#' - \textrm{auc.min}}\Big)}{0.5 
#' (1 + (partialAUC - auc.min) / (auc.max - auc.min))}
#' Here auc.min is the AUC achieved by the non-discriminate classifier and auc.max is the AUC
#' achieved by a perfect classifier. Thus, a non-discriminative classifier and a perfect one will 
#' always have partial AUCs of 0.5 and 1 respectively. 
#' Unfortunately, the corrected partial AUC can not be interpreted in a meaningful way if the curve
#' in the area of interest is below the non-discriminate classifier. Those partial AUC values can
#' be recognized by being below 0.5. For this reason, fbroc will give a warning if bootstrapping
#' produces corrected partial AUC values below 0.5.
#' @references Donna Katzman McClish. (1989). \emph{Analyzing a Portion of the ROC Curve.}
#' Medical Decision Making, \url{http://mdm.sagepub.com/content/9/3/190.abstract}. 
