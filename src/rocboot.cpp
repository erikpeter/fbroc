#include <Rcpp.h>
using namespace Rcpp;

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

// Count new tprs and fpr at each threshold
IntegerVector get_tpr_fpr_delta(IntegerVector tpr_fpr_delta_index, int n_thres, int n) {
   IntegerVector tpr_fpr_delta(2*n_thres);
     for (int i = 0; i < n; i++) {
    tpr_fpr_delta[tpr_fpr_delta_index[i]]++;
  }
  return tpr_fpr_delta;
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

// calculate tpr_fpr from Index
NumericVector build_tpr_fpr(IntegerVector tpr_fpr_index, int n_pos, 
                          int n_neg, int n_thres) {
  
  int n = n_pos + n_neg;
  
  IntegerVector tpr_fpr_delta = get_tpr_fpr_delta(tpr_fpr_index, n_thres, n);
  IntegerVector tpr_fpr = get_tpr_fpr(tpr_fpr_delta, n_thres, n_pos, n_neg);
  NumericVector tpr_fpr_rate = get_tpr_fpr_rate(tpr_fpr, n_thres, n_pos, n_neg);
  
  return tpr_fpr_rate;
}

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
  
  for (int i = 0; i < n_boot; i++) {
    // shuffle first
    
    IntegerVector boot_tpr_fpr_index = 
      get_boot_tpr_fpr_index(pos_index, neg_index);
    NumericVector boot_tpr_fpr = 
      build_tpr_fpr(boot_tpr_fpr_index, n_pos, n_neg, n_thres);
    // copy into results matrix
    for (int j = 0; j < 2*n_thres; j++) {
      tpr_fpr_matrix(i, j) = boot_tpr_fpr[j];
    }    
  }
  
  return tpr_fpr_matrix;
}

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

