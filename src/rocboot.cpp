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

NumericVector get_tpr_vector(NumericVector tpr_fpr_vec, 
                             NumericVector steps,
                             int n_steps,
                             int n_thres) {
  NumericVector tpr_vec (n_steps + 1);

  int j = 0;
  for (int i = 0; i <= n_steps; i++) {
    while ((j < n_thres) && 
           (tpr_fpr_vec[n_thres + j] >= steps[i])) {
             j++;
           }
    tpr_vec[i] = tpr_fpr_vec[j];       
  }
  return tpr_vec;
}

// [[Rcpp::export]]
NumericMatrix get_tpr_matrix(NumericMatrix tpr_fpr, int n_steps) {
  double step_size = (1.0 / n_steps);
  NumericVector steps (n_steps + 1);
  int n_boot = tpr_fpr.nrow();
  int n_thres = tpr_fpr.ncol()/2;

  NumericMatrix tpr_matrix (n_boot, n_steps + 1);
  
  for (int i = 0; i <= n_steps; i++) steps[i] = 1. - i * step_size;
  
  // use the order of both TPR and FPR to find the relevant indices
  // more quickly
  for (int j = 0; j < n_boot; j++) {
    tpr_matrix(j, _) = get_tpr_vector(tpr_fpr(j, _),
                                      steps,
                                      n_steps,
                                      n_thres);
  }

  return tpr_matrix;
}

// [[Rcpp::export]]
NumericMatrix get_tpr_matrix_uncached(NumericVector pred, 
                                      IntegerVector true_class, 
                                      NumericVector thres, 
                                      int n_boot, 
                                      int n_steps) {
  
  double step_size = (1.0 / n_steps);
  NumericVector steps (n_steps + 1);
  int n_thres = thres.size();
  for (int i = 0; i <= n_steps; i++) steps[i] = 1. - i * step_size;
  
  
  NumericMatrix tpr_matrix (n_boot, n_steps + 1);
  
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
    tpr_matrix(i, _) = get_tpr_vector(one_iteration,
                                      steps,
                                      n_steps,
                                      n_thres);
  }
  
  return tpr_matrix;                                        
}



