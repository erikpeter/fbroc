#' @section Note on partial AUC correction:
#' 
#' The partial AUC is hard to interpret without considering the range on which it is calculated.
#' Not only does the partial AUC scale with the width of the interval over which it is calculated,
#' but it also depends on where the interval is located.
#' For example, if the ROC Curve is integrated over the FPR interval [0, 0.1] a completely random
#' and non-discrimate classifier would have a partial AUC of 0.05, but the same ROC curve integrated over
#' the interval [0.9, 1] would yield a partial AUC of 0.95.
#' 
#' The correction by McClish produces a corrected partial AUC given by:
#' \deqn{\frac{1}{2} \Big(1 + \frac{\textrm{partialAUC} - \textrm{auc.min}}{\textrm{auc.max} 
#' - \textrm{auc.min}}\Big)}{0.5 
#' (1 + (partialAUC - auc.min) / (auc.max - auc.min))}
#' Here auc.min is the AUC achieved by the non-discriminate classifier and auc.max is the AUC
#' achieved by a perfect classifier. Thus, a non-discriminative classifier will always have an AUC
#' of 0.5 and a perfect one classifier will always have a partial AUCs of 1. 
#' 
#' Unfortunately, the corrected partial AUC cannot be interpreted in a meaningful way if the curve
#' is below the non-discriminate classifier, producing corrected partial AUCs values below 0.5. 
#' For this reason, fbroc will give a warning if the bootstrap produces corrected 
#' partial AUC values below 0.5.
#' @references Donna Katzman McClish. (1989). \emph{Analyzing a Portion of the ROC Curve.}
#' Medical Decision Making, \url{http://mdm.sagepub.com/content/9/3/190.abstract}. 
