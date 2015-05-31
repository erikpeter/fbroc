#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"
#include "sampler.h"
#include "paired.roc.h"
#include "performance.h"
#include "interface.common.h"

// [[Rcpp::export]]
List paired_roc_analysis(NumericVector pred1, NumericVector pred2, IntegerVector true_class) {
  List out(2);  
  out[0] = roc_analysis(pred1, true_class);
  out[1] = roc_analysis(pred2, true_class);
  return out;
}