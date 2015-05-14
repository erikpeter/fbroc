#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "bootstrapper.h"

// [[Rcpp::export]]
List roc_analysis(NumericVector pred, IntegerVector true_class) {
  ROC roc (pred, true_class);
  NumericVector original_tpr = roc.get_tpr();
  NumericVector original_fpr = roc.get_fpr();
  NumericVector thres = roc.get_thresholds();
  List out(3);  
  out[0] = original_tpr;
  out[1] = original_fpr;
  out[2] = thres;
  return out;
}

// [[Rcpp::export]]
List tpr_fpr_boot2(NumericVector pred, IntegerVector true_class, int n_boot) {
  Sampler_base *sampler = new Sampler_Stratified(true_class);
  sampler->generate();
  //Bootstrapped_ROC boot_roc (pred, true_class);
  NumericVector original_tpr = ROC(pred, true_class).get_tpr();
  int n_thres = original_tpr.size();
  // NumericMatrix tpr (n_boot, n_thres);
  //NumericMatrix fpr (n_boot, n_thres);
  for (int i = 0; i < n_boot; i++) {
    //boot_roc.bootstrap();
    //tpr(i, _) = boot_roc.get_tpr();
    //fpr(i, _) = boot_roc.get_fpr();
  }
  List out(3);
  out[0] = original_tpr;
  out[1] = n_thres;
  out[2] = sampler->get_shuffled_index(true);
  return out;
}