// Preparations before bootstrap and work on original data

#include <Rcpp.h>
using namespace Rcpp;

#include "calc_shuffled.h"

// Builds the original tpr_fpr index for later usage
IntegerVector get_tpr_fpr_index(NumericVector pred, IntegerVector true_class, NumericVector thres) {
   int n = pred.size();
   int n_thres = thres.size();
   IntegerVector out(n);
   
   for (int i = 0; i < n; i++) {
     int j = 0;
     while (pred[i] >= thres[j]) j++;
     if (true_class[i] == 1) out[i] = j;
     else out[i] = j + n_thres;
   }
   return out;
}

// find which indices belong to a specific class
IntegerVector get_class_index(IntegerVector index, IntegerVector true_class,
                              int which_class, int n_class) {
  IntegerVector class_index (n_class);
  int n = index.size();
  int j = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == which_class) {
      class_index[j++] = index[i];
    }
  }  
  return class_index;
}

// [[Rcpp::export]]
NumericVector true_tpr_fpr(NumericVector pred, IntegerVector true_class,
                           NumericVector thres) {
  int n_thres = thres.size();
  int n = pred.size();
  // do same as with bootstrap just don`t resample
  int n_pos = 0;
  int n_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
      else n_neg++;
  }
  IntegerVector tpr_fpr_index = get_tpr_fpr_index(pred, true_class, thres);
  NumericVector tpr_fpr = build_tpr_fpr(tpr_fpr_index, n_pos, n_neg, n_thres);
  
  return tpr_fpr;
}

// [[Rcpp::export]]
IntegerVector find_thresholds(NumericVector pred, IntegerVector true_class) {
  int n_pred = pred.size();
  IntegerVector is_threshold (n_pred);
  
  is_threshold[0] = 1;
  is_threshold[n_pred - 1] = 1;

  bool seen_pos = false;
  bool seen_neg = false;
  /* 
  always use smallest and largest value of pred as threshold
  thresholds happen when both positive and negative classes have been
  observed since the last threshold
  */
  for (int i = 0; i < n_pred; i++) { 
    if (true_class[i] == 1) seen_pos = true;
      else seen_neg = true;
    if (seen_pos && seen_neg) {
      is_threshold[i] = 1;
      if (true_class[i] == 1) seen_neg = false;
        else seen_pos = false;
    }
  }
  return is_threshold;
}

