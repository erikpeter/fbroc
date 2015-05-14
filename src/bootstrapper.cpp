#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "bootstrapper.h"


NumericVector Bootstrapped_ROC::get_tpr(){
  NumericVector out = shuffled.get_tpr();
  return out;
}

NumericVector Bootstrapped_ROC::get_fpr(){
  NumericVector out = shuffled.get_fpr();
  return out;
}

void Bootstrapped_ROC::bootstrap() {
  sampler->generate();
  IntegerVector shuffled_pos_index = sampler->get_shuffled_index(true);
  IntegerVector shuffled_neg_index = sampler->get_shuffled_index(false);
  shuffled = Shuffled_ROC(roc, shuffled_pos_index, shuffled_neg_index);
}

Bootstrapped_ROC::Bootstrapped_ROC(NumericVector pred, IntegerVector true_class)
{
  roc = ROC(pred, true_class);
  sampler = new Sampler_Stratified(true_class);
  shuffled = Shuffled_ROC();
}

Bootstrapped_ROC::~Bootstrapped_ROC()
{
  delete sampler;
}
