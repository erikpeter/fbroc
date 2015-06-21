#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "interface.common.h"
#include "performance.h"

// [[Rcpp::export]]
List roc_analysis(NumericVector pred, IntegerVector true_class) {
  ROC roc (pred, true_class);
  NumericVector &original_tpr = roc.get_tpr();
  NumericVector &original_fpr = roc.get_fpr();
  NumericVector &thres = roc.get_thresholds();
  List out(4);  
  out[0] = original_tpr;
  out[1] = original_fpr;
  out[2] = thres;
  NumericVector dummy_param (0);
  out[3] = get_perf_auc(original_tpr, original_fpr, dummy_param);
  return out;
}

PerfFun pick_measure(Measure measure) {
  PerfFun out;
  if (measure == AUC) out = &get_perf_auc; 
  if (measure == TPR_AT_FPR) out = &get_tpr_at_fixed_fpr; 
  if (measure == FPR_AT_TPR) out = &get_fpr_at_fixed_tpr; 
  return out;
}