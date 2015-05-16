#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "bootstrapper.h"
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
  out[3] = get_perf_auc(original_tpr, original_fpr);
  return out;
}

// [[Rcpp::export]]
List tpr_fpr_boot2(NumericVector pred, IntegerVector true_class, int n_boot) {

  Bootstrapped_ROC boot_roc (pred, true_class);
  int n_thres = boot_roc.get_n_thres();
  NumericMatrix tpr (n_boot, n_thres);
  NumericMatrix fpr (n_boot, n_thres);
  for (int i = 0; i < n_boot; i++) {
    boot_roc.bootstrap();
    tpr(i, _) = boot_roc.get_tpr();
    fpr(i, _) = boot_roc.get_fpr();
  }
  List out(2);
  out[0] = tpr;
  out[1] = fpr;
  return out;
}

// [[Rcpp::export]]
NumericVector get_uncached_perf(NumericVector pred, IntegerVector true_class, 
                                int n_boot, int measure) {
  double (*choosen_measure)(NumericVector , NumericVector );  
  if (measure == 0) choosen_measure = &get_perf_auc; 
  Bootstrapped_ROC boot_roc (pred, true_class);
  NumericVector roc_perf = NumericVector (n_boot);
  for (int i = 0; i < n_boot; i++) {
    boot_roc.bootstrap();
    roc_perf[i] = choosen_measure(boot_roc.get_tpr(), boot_roc.get_fpr());
  }
  return roc_perf;
}

// [[Rcpp::export]]
NumericVector get_cached_perf(NumericMatrix tpr, NumericMatrix fpr, int measure) {
  double (*choosen_measure)(NumericVector, NumericVector);
  int n_boot = tpr.nrow();
  NumericVector roc_perf = NumericVector (n_boot);
  
  if (measure == 0) choosen_measure = &get_perf_auc; 
  //iterate over bootstrap replicates and get performance for each   
  for (int i = 0; i < n_boot; i++) {
    double perf = choosen_measure(tpr(i, _), fpr(i, _));
    roc_perf[i] = perf;
  }
  
  return roc_perf;
}
