#include <Rcpp.h>
using namespace Rcpp;

#include "performance.h"

double get_perf_auc(NumericVector &tpr, NumericVector &fpr, NumericVector &param) {
  int n_thres = tpr.size();
  double auc = 0.;
  // Numerical integration of step functions is easy
  for (int j = 1; j < n_thres; j++) {
    auc += (tpr[j - 1] - tpr[j]) * (1 - fpr[j - 1]);
  }
  return auc;
}
