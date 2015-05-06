// performance measures and get performance code

#include <Rcpp.h>
using namespace Rcpp;

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
