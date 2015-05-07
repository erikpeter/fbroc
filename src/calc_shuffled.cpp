// Calculates tpr fpr for bootstrap iteration

#include <Rcpp.h>
using namespace Rcpp;

// Directly resample the tpr_fpr_index to save time
IntegerVector get_boot_tpr_fpr_index(IntegerVector pos_index, 
                                     IntegerVector neg_index) {
  int n_pos = pos_index.size();
  int n_neg = neg_index.size();
  int n = n_pos + n_neg;
  IntegerVector boot_tpr_fpr_index(n);
  
  int j = 0;
  
  NumericVector random_pos = runif(n_pos);
  NumericVector random_neg = runif(n_neg);
  // use stratified bootstrapping
  for (int i = 0; i < n_pos; i++) {
    int random_index = (int)(n_pos * random_pos[i]);
    boot_tpr_fpr_index[j++] = pos_index[random_index];
  }
  for (int i = 0; i < n_neg; i++) {
    int random_index = (int)(n_neg * random_neg[i]);
    boot_tpr_fpr_index[j++] = neg_index[random_index];
  }
    
  return boot_tpr_fpr_index;
}

// Divided by n_pos respectively n_neg to go from tpr_fpr to tpr_fpr_rate
NumericVector get_tpr_fpr_rate(IntegerVector tpr_fpr, int n_thres, int n_pos, int n_neg) {
  double inv_pos = 1. / n_pos;
  double inv_neg = 1. / n_neg;
  
  NumericVector tpr_fpr_rate(2*n_thres); 
    for (int i = 0; i < n_thres; i++) {
    tpr_fpr_rate[i] = inv_pos * tpr_fpr[i];
    tpr_fpr_rate[i + n_thres] = inv_neg * tpr_fpr[i + n_thres];
  }
  
  return tpr_fpr_rate;
}

// Count cumulative tprs and fpr at each threshold
IntegerVector get_tpr_fpr(IntegerVector tpr_fpr_delta, int n_thres, int n_pos, int n_neg) {
  IntegerVector tpr_fpr(2*n_thres);
  tpr_fpr[0] = n_pos;
  tpr_fpr[n_thres] = n_neg;
  
  for (int i = 1; i < n_thres; i++) {
    tpr_fpr[i] = tpr_fpr[i - 1] - tpr_fpr_delta[i];
    tpr_fpr[i + n_thres] = tpr_fpr[n_thres + i - 1] - tpr_fpr_delta[n_thres + i];
  }
  
  return tpr_fpr;
}

// Count new tprs and fpr at each threshold
IntegerVector get_tpr_fpr_delta(IntegerVector tpr_fpr_delta_index, int n_thres, int n) {
   IntegerVector tpr_fpr_delta(2*n_thres);
     for (int i = 0; i < n; i++) {
    tpr_fpr_delta[tpr_fpr_delta_index[i]]++;
  }
  return tpr_fpr_delta;
}

// calculate tpr_fpr from Index
NumericVector build_tpr_fpr(IntegerVector tpr_fpr_index, int n_pos, 
                          int n_neg, int n_thres) {
  
  int n = n_pos + n_neg;
  
  IntegerVector tpr_fpr_delta = get_tpr_fpr_delta(tpr_fpr_index, n_thres, n);
  IntegerVector tpr_fpr = get_tpr_fpr(tpr_fpr_delta, n_thres, n_pos, n_neg);
  NumericVector tpr_fpr_rate = get_tpr_fpr_rate(tpr_fpr, n_thres, n_pos, n_neg);
  
  return tpr_fpr_rate;
}

// [[Rcpp::export]]
NumericVector tpr_fpr_boot_iterate(int n_thres, 
                                   IntegerVector tpr_fpr_index,
                                   IntegerVector pos_index,
                                   IntegerVector neg_index) {
  int n_pos = pos_index.size();
  int n_neg = neg_index.size();
  IntegerVector boot_tpr_fpr_index = 
    get_boot_tpr_fpr_index(pos_index, neg_index);
  NumericVector boot_tpr_fpr = 
    build_tpr_fpr(boot_tpr_fpr_index, n_pos, n_neg, n_thres);
  return boot_tpr_fpr;    
}

