//contains main functions called from R

#include <Rcpp.h>
using namespace Rcpp;

#include "calc_shuffled.h"
#include "calc_perf.h"
#include "prep_roc.h"



/*
//' @export
// [[Rcpp::export]]
IntegerMatrix test_random(NumericVector pred, IntegerVector true_class, 
                           int n_boot) {
  
  int n = pred.size();
  
  int n_pos = 0;
  int n_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
      else n_neg++;
  }
    
  IntegerMatrix boot_matrix(n_boot, n);
    
  for (int i = 0; i < n_boot; i++) {
    // shuffle first
    
  NumericVector random_pos = runif(n_pos);
  NumericVector random_neg = runif(n_neg);
  // use stratified bootstrapping
  int k = 0;
  for (int j = 0; j < n_pos; j++) {
     boot_matrix(i, k++) = (int)(n_pos * random_pos[j]);
  }
  for (int j = 0; j < n_neg; j++) {
     boot_matrix(i, k++) = (int) (n_neg*random_neg[j]);
  }
 
  }
  
  return boot_matrix;
}
*/

// [[Rcpp::export]]
NumericMatrix tpr_fpr_boot(NumericVector pred, IntegerVector true_class, 
                           NumericVector thres, int n_boot) {
  
  int n_thres = thres.size();

  IntegerVector n_class = count_classes(true_class);

  NumericMatrix tpr_fpr_matrix(n_boot, 2*n_thres);
  IntegerVector tpr_fpr_index = get_tpr_fpr_index(pred, true_class, thres);
  
  // get class specific indices for shuffling later and initialise random number
  // generator
  IntegerVector pos_index = get_class_index(tpr_fpr_index, true_class, 
                                            1, n_class[1]);
  IntegerVector neg_index = get_class_index(tpr_fpr_index, true_class, 
                                            0, n_class[0]);
  
  for (int i = 0; i < n_boot; i++) {
    // shuffle first
    NumericVector boot_tpr_fpr = tpr_fpr_boot_iterate(n_thres,
                                                     tpr_fpr_index,
                                                     pos_index,
                                                     neg_index);
    // copy into results matrix
    tpr_fpr_matrix(i, _) = boot_tpr_fpr;
  }
  
  return tpr_fpr_matrix;
}

// [[Rcpp::export]]
NumericVector get_tpr_matrix(NumericMatrix tpr_fpr, int n_steps) {
  double step_size = (1.0 / n_steps);
  NumericVector steps (n_steps + 1);
  int n_boot = tpr_fpr.nrow();
  int n_thres = tpr_fpr.ncol()/2;
  
  IntegerVector boot_index (n_boot);  
  NumericMatrix tpr_matrix (n_boot, n_steps + 1);
  
  for (int i = 0; i <= n_steps; i++) steps[i] = 1. - i * step_size;
  
  // use the order of both TPR and FPR to find the relevant indices
  // more quickly
  for (int i = 0; i <= n_steps; i++) {
    for (int j = 0; j < n_boot; j++) {
      while ((boot_index[j] < n_thres) && 
            (tpr_fpr(j, n_thres + boot_index[j]) >= steps[i])) {
              boot_index[j] = boot_index[j] + 1;
            }
      tpr_matrix(j, i) = tpr_fpr(j, boot_index[j] - 1);
    }
  }
  
  return tpr_matrix;
}





