#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "paired.roc.h"
#include "performance.h"
#include "interface.common.h"

// [[Rcpp::export]]
List paired_roc_analysis(NumericVector pred1, NumericVector pred2, IntegerVector true_class) {
  List out(2);  
  out[0] = roc_analysis(pred1, true_class);
  out[1] = roc_analysis(pred2, true_class);
  return out;
}

// [[Rcpp::export]]
List get_uncached_perf_paired(NumericVector pred1, 
                                       NumericVector pred2,
                                       IntegerVector true_class, 
                                       NumericVector param,
                                       int n_boot, int measure) {
  PerfFun choosen_measure = pick_measure(static_cast <Measure>(measure));                                
  Bootstrapped_paired_ROC boot_roc (pred1, pred2, true_class);
  NumericVector roc_perf1 = NumericVector (n_boot);
  NumericVector roc_perf2 = NumericVector (n_boot);
  for (int i = 0; i < n_boot; i++) {
    boot_roc.bootstrap();
    roc_perf1[i] = choosen_measure(boot_roc.get_roc(0).get_tpr(), boot_roc.get_roc(0).get_fpr(), param);
    roc_perf2[i] = choosen_measure(boot_roc.get_roc(1).get_tpr(), boot_roc.get_roc(1).get_fpr(), param);
  }
  List out(2); 
  out[0] = roc_perf1;
  out[1] = roc_perf2;
  return out;
}

// [[Rcpp::export]]
NumericMatrix tpr_at_fpr_delta_uncached(NumericVector pred1, 
                                        NumericVector pred2, 
                                        IntegerVector true_class,
                                        int n_boot,
                                        int n_steps) {
  Bootstrapped_paired_ROC boot_roc (pred1, pred2, true_class);
  NumericVector steps = get_steps(n_steps);
  NumericMatrix tpr_matrix (n_boot, n_steps + 1);
  for (int j = 0; j < n_boot; j++) { 
    boot_roc.bootstrap();
    tpr_matrix(j, _) = boot_roc.get_roc(0).get_tpr_at_fpr(steps) - 
                       boot_roc.get_roc(1).get_tpr_at_fpr(steps);
  }
  return tpr_matrix;
}