#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "bootstrapper.h"

// [[Rcpp::export]]
List original_tpr_fpr(NumericVector pred, IntegerVector true_class) {
  ROC roc (pred, true_class);
  NumericVector original_tpr = roc.tpr();
  NumericVector original_fpr = roc.fpr();
  List out(2);
  
  out[0] = original_tpr;
  out[1] = original_fpr;
  return out;
}

// [[Rcpp::export]]
IntegerVector test_fun(NumericVector pred, IntegerVector true_class, int n_boot) {
  Sampler_Stratified sampler (true_class);
  sampler.generate();
  return sampler.get_shuffled_index(true);
}
// [[Rcpp::export]]
List tpr_fpr_boot2(NumericVector pred, IntegerVector true_class, int n_boot) {
  Sampler_base *sampler = new Sampler_Stratified(true_class);
  sampler->generate();
  //Bootstrapped_ROC boot_roc (pred, true_class);
  NumericVector original_tpr = ROC(pred, true_class).tpr();
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