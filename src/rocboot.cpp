#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)


//' generate
//'
//' @export
// [[Rcpp::export]]
IntegerVector tpr_fpr_delta(NumericVector pred, IntegerVector true_class, NumericVector thres) {
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

//' ok
//' @export
// [[Rcpp::export]]
NumericVector tpr_fpr(NumericVector pred, IntegerVector true_class, NumericVector thres) {
  IntegerVector tpr_fpr_delta_index = tpr_fpr_delta(pred, true_class, thres);
  int n_thres = thres.size();
  int n = pred.size();
  int n_pos = 0;
  int n_neg = 0;
  
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
    else n_neg++;
  }
  
  double inv_pos = 1. / n_pos;
  double inv_neg = 1. / n_neg;
  
  NumericVector tpr_fpr(2*n_thres);
  tpr_fpr[0] = 1;
  tpr_fpr[n_thres] = 1;
  NumericVector tpr_fpr_delta_sum(2*n_thres);
  for (int i = 0; i < n; i++) {
    tpr_fpr_delta_sum[tpr_fpr_delta_index[i]]++;
  }
  for (int i = 1; i < n_thres; i++) {
    tpr_fpr[i] = tpr_fpr[i - 1] - inv_pos*tpr_fpr_delta_sum[i];
    tpr_fpr[i + n_thres] = tpr_fpr[n_thres + i - 1] - inv_neg*tpr_fpr_delta_sum[n_thres + i];
  }
  return tpr_fpr;
}

double auc(NumericVector tpr_fpr) {
  out = 0.;
  return out;
}
