// performance measures and get performance code

#include <Rcpp.h>
using namespace Rcpp;

#include "prep_roc.h"
#include "calc_shuffled.h"

// [[Rcpp::export]]
double get_auc(NumericVector tpr_fpr) {
  int n_thres = tpr_fpr.size()/2;
  double auc = 0.;
  // Numerical integration of step functions is easy
  for (int j = 1; j < n_thres; j++) {
    auc += (tpr_fpr[j - 1] - tpr_fpr[j]) * (1 - tpr_fpr[n_thres + j - 1]);
  }
  return auc;
}

// [[Rcpp::export]]
NumericVector get_roc_perf(NumericMatrix tpr_fpr, int measure) {
  double (*choosen_measure)(NumericVector);
  int n_boot = tpr_fpr.nrow();
  NumericVector roc_perf = NumericVector (n_boot);
  
  if (measure == 0) choosen_measure = &get_auc; 
  //iterate over bootstrap replicates and get performance for each   
  for (int i = 0; i < n_boot; i++) {
    NumericVector one_iteration = tpr_fpr(i, _);
    double perf = choosen_measure(one_iteration);
    roc_perf[i] = perf;
  }
  
  return roc_perf;
}

// [[Rcpp::export]]
NumericVector get_roc_perf_uncached(NumericVector pred, 
                                    IntegerVector true_class,
                                    NumericVector thres,
                                    int measure, 
                                    int n_boot) {
  int n_thres = thres.size();
  double (*choosen_measure)(NumericVector);
  NumericVector roc_perf = NumericVector (n_boot);
  if (measure == 0) choosen_measure = &get_auc; 
  
  IntegerVector tpr_fpr_index = get_tpr_fpr_index(pred, true_class, thres);
  IntegerVector n_class = count_classes(true_class);
  
  IntegerVector pos_index = get_class_index(tpr_fpr_index, true_class, 
                                            1, n_class[1]);
  IntegerVector neg_index = get_class_index(tpr_fpr_index, true_class, 
                                            0, n_class[0]);
     
  for (int i = 0; i < n_boot; i++) {
    NumericVector one_iteration = tpr_fpr_boot_iterate(n_thres,
                                                       tpr_fpr_index,
                                                       pos_index,
                                                       neg_index);
    double perf = choosen_measure(one_iteration);
    roc_perf[i] = perf;
  }
  
  return roc_perf;
  
}