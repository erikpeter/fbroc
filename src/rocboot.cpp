#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)


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

//' ok
//' @export
// [[Rcpp::export]]
NumericVector tpr_fpr(NumericVector pred, IntegerVector true_class, NumericVector thres) {
  IntegerVector tpr_fpr_delta_index = get_tpr_fpr_index(pred, true_class, thres);
  int n_thres = thres.size();
  int n = pred.size();
  int n_pos = 0;
  int n_neg = 0;
  
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
    else n_neg++;
  }

  IntegerVector tpr_fpr_delta = get_tpr_fpr_delta(tpr_fpr_delta_index, n_thres, n);
  IntegerVector tpr_fpr = get_tpr_fpr(tpr_fpr_delta, n_thres, n_pos, n_neg);
  NumericVector tpr_fpr_rate = get_tpr_fpr_rate(tpr_fpr, n_thres, n_pos, n_neg);
  
  return tpr_fpr_rate;
}

double auc(NumericVector tpr_fpr) {
  double out = 0.;
  return out;
}
