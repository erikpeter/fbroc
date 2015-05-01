#include <Rcpp.h>
#include "randomc.h"
#include "sfmt.h"

using namespace Rcpp;

//' generate
//'
//' @export
// [[Rcpp::export]]
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

IntegerVector get_tpr_fpr_delta(IntegerVector tpr_fpr_delta_index, int n_thres, int n) {
   IntegerVector tpr_fpr_delta(2*n_thres);
     for (int i = 0; i < n; i++) {
    tpr_fpr_delta[tpr_fpr_delta_index[i]]++;
  }
  return tpr_fpr_delta;
}

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


NumericVector build_tpr_fpr(IntegerVector tpr_fpr_index, int n_pos, 
                          int n_neg, int n_thres) {
  
  int n = n_pos + n_neg;
  
  IntegerVector tpr_fpr_delta = get_tpr_fpr_delta(tpr_fpr_index, n_thres, n);
  IntegerVector tpr_fpr = get_tpr_fpr(tpr_fpr_delta, n_thres, n_pos, n_neg);
  NumericVector tpr_fpr_rate = get_tpr_fpr_rate(tpr_fpr, n_thres, n_pos, n_neg);
  
  return tpr_fpr_rate;
}

IntegerVector get_boot_tpr_fpr_index(IntegerVector pos_index, 
                                     IntegerVector neg_index,
                                     CRandomSFMT &random_gen) {
  int n_pos = pos_index.size();
  int n_neg = neg_index.size();
  int n = n_pos + n_neg;
  IntegerVector boot_tpr_fpr_index(n);
  
  int j = 0;
 
  for (int i = 0; i < n_pos; i++) {
    int random_index = random_gen.IRandomX(0, n_pos - 1);
    boot_tpr_fpr_index[j++] = pos_index[random_index];
  }
  for (int i = 0; i < n_neg; i++) {
    int random_index = random_gen.IRandomX(0, n_neg - 1);
    boot_tpr_fpr_index[j++] = neg_index[random_index];
  }
    
  return boot_tpr_fpr_index;
}

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

//' ok
//' @export
// [[Rcpp::export]]
NumericVector true_tpr_fpr(NumericVector pred, IntegerVector true_class,
                           NumericVector thres) {
  int n_thres = thres.size();
  int n = pred.size();
  
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


//' ok
//' @export
// [[Rcpp::export]]
NumericMatrix tpr_fpr_boot(NumericVector pred, IntegerVector true_class, 
                           NumericVector thres, int n_boot, int seed) {
  
  int n_thres = thres.size();
  int n = pred.size();
  
  int n_pos = 0;
  int n_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
      else n_neg++;
  }
    
  NumericMatrix tpr_fpr_matrix(n_boot, 2*n_thres);
  IntegerVector tpr_fpr_index = get_tpr_fpr_index(pred, true_class, thres);
  
  // get class specific indices for shuffling later and initialise random number
  // generator
  IntegerVector pos_index = get_class_index(tpr_fpr_index, true_class, 1, n_pos);
  IntegerVector neg_index = get_class_index(tpr_fpr_index, true_class, 0, n_neg);
  CRandomSFMT random_gen = CRandomSFMT(seed);
  
  for (int i = 0; i < n_boot; i++) {
    // shuffle first
    
    IntegerVector boot_tpr_fpr_index = 
      get_boot_tpr_fpr_index(pos_index, neg_index, random_gen);
    NumericVector boot_tpr_fpr = 
      build_tpr_fpr(boot_tpr_fpr_index, n_pos, n_neg, n_thres);
    // copy into results matrix
    for (int j = 0; j < 2*n_thres; j++) {
      tpr_fpr_matrix(i, j) = boot_tpr_fpr[j];
    }    
  }
  
  return tpr_fpr_matrix;
}

//' @export
// [[Rcpp::export]]
NumericVector get_auc(NumericMatrix tpr_fpr) {
  int n_boot = tpr_fpr.nrow();
  int n_thres = tpr_fpr.ncol()/2;
  NumericVector auc (n_boot);
  for (int i = 0; i < n_boot; i++) {
    for (int j = 1; j < n_thres; j++) {
      auc[i] += (tpr_fpr(i, j - 1) - tpr_fpr(i, j)) * (1 - tpr_fpr(i, n_thres + j - 1));
    }
  }
  return auc;
}


