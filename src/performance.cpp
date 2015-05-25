#include <Rcpp.h>
using namespace Rcpp;

#include "performance.h"

double get_perf_auc(NumericVector &tpr, NumericVector &fpr, NumericVector &param) 
{
  int n_thres = tpr.size();
  double auc = 0.;
  // Numerical integration of step functions is easy
  for (int j = 1; j < n_thres; j++) {
    auc += (tpr[j - 1] - tpr[j]) * (2 - fpr[j - 1] - fpr[j]);
  }
  auc = 0.5 * auc;
  return auc;
}

double get_tpr_at_fixed_fpr(NumericVector &tpr, NumericVector &fpr, NumericVector &param) 
{
  double at = param[0];
  //double out = 0;
  if (at == 1) return param[0];  
  int i = 0;  
  while (fpr[i++] > at);
  //if (fpr[i] == at) out = tpr[i];
  //else
  double out = tpr[i-1];    
  return out;
}

double get_fpr_at_fixed_tpr(NumericVector &tpr, NumericVector &fpr, NumericVector &param) 
{
  double at = param[0];
  if (at == 0) return param[0];
  int i = tpr.size() - 1;  
  while (tpr[i--] < at);
  double out = fpr[i+1];  
  return out;
}