#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"


void ROC::strat_shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg) {
  IntegerVector temp = index_pos;
  IntegerVector temp2 = index_neg;
  
  index_pos = new_index_pos;
  index_neg = new_index_neg;
  new_index_pos = temp;
  new_index_neg = temp2;  
    
  for (int i = 0; i < n_pos; i++) new_index_pos[i] = index_pos[shuffle_pos[i]];
  for (int i = 0; i < n_neg; i++) new_index_neg[i] = index_neg[shuffle_neg[i]];
  
  temp = index_pos;
  temp2 = index_neg;
  index_pos = new_index_pos;
  index_neg = new_index_neg;
  new_index_pos = temp;
  new_index_neg = temp2;  
  reset_delta();
  get_positives_delta();
  get_positives();
  get_rate();
}


void ROC::shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg) {
  n_pos = shuffle_pos.size();
  n_neg = shuffle_neg.size();
  IntegerVector new_index_pos(n_pos);
  IntegerVector new_index_neg(n_neg);
  for (int i = 0; i < n_pos; i++) new_index_pos[i] = index_pos[shuffle_pos[i]];
  for (int i = 0; i < n_neg; i++) new_index_neg[i] = index_neg[shuffle_neg[i]];
  index_pos = new_index_pos;
  index_neg = new_index_neg;
  reset_delta();
  get_positives_delta();
  get_positives();
  get_rate();
}

void ROC::get_positives()  {  
  for (int i = 1; i < n_thresholds; i++) {
    true_positives[i] = true_positives[i - 1] - delta_pos[i];
    false_positives[i] = false_positives[i - 1] - delta_neg[i];
  }
}

void ROC::reset_delta() {
   for (int i = 0; i < n_thresholds; i++) {
    delta_pos[i] = 0;
    delta_neg[i] = 0;
  }
}

void ROC::get_positives_delta()
{
  for (int i = 0; i < n_pos; i++) {
    delta_pos[index_pos[i]]++;
  }
  for (int i = 0; i < n_neg; i++) {
    delta_neg[index_neg[i]]++;
  }
}

void ROC::get_rate()
{  
  double mult_pos = 1. / n_pos;
  double mult_neg = 1. / n_neg;
    
  for (int i = 0; i < n_thresholds; i++) {
    tpr[i] = mult_pos * true_positives[i];
    fpr[i] = mult_neg * false_positives[i];
  }
}

NumericVector ROC::get_thresholds() const 
{
  return thresholds;
}

NumericVector ROC::get_tpr() const
{
  NumericVector out = tpr;
  return out;
}

NumericVector ROC::get_fpr() const
{
  NumericVector out = fpr;
  return out;
}


IntegerVector ROC::build_index(NumericVector pred)
{
  IntegerVector index(pred.size());
  for (int i = 0; i < pred.size(); i++) {
    int j = 0;
    while (pred[i] >= thresholds[j]) j++;
    index[i] = j;
  }
  return index;
}

void ROC::build_pred(NumericVector pred, IntegerVector true_class)
{
  pred_pos = NumericVector(n_pos);
  pred_neg = NumericVector(n_neg);
  int index_pos = 0;
  int index_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) pred_pos[index_pos++] = pred[i];
    else pred_neg[index_neg++] = pred[i];
  }
}

void ROC::find_thresholds(NumericVector pred, IntegerVector true_class) {
  LogicalVector is_threshold (n);
  is_threshold[0] = true;
  bool seen_pos = false;
  bool seen_neg = false;
  n_thresholds = 1;
  
  for (int i = 0; i < n; i++) { 
    if (true_class[i] == 1) seen_pos = true;
      else seen_neg = true;
    if (seen_pos && seen_neg) {
      is_threshold[i] = true;
      n_thresholds++;
      if (true_class[i] == 1) seen_neg = false;
        else seen_pos = false;
    }
  }
  NumericVector thres (n_thresholds + 1);
  int j = 0;
  for (int i = 0; i < n; i++) {
    if (is_threshold[i]) {
      thres[j++] = pred[i];  
    }
  }
  thres[n_thresholds++] = pred[n - 1] + 1.;
  thresholds = thres;
}

int ROC::get_n_thres() const {
  return n_thresholds;
}

ROC::ROC(NumericVector pred, IntegerVector true_class)
{
  n = pred.size();
  n_pos = 0;
  n_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
    else n_neg++;
  }  
  find_thresholds(pred, true_class);
  build_pred(pred, true_class);
  
  index_pos = build_index(pred_pos);
  index_neg = build_index(pred_neg);
  new_index_pos = build_index(pred_pos);
  new_index_neg = build_index(pred_neg);

  delta_pos = IntegerVector (n_thresholds);
  delta_neg = IntegerVector (n_thresholds);
  true_positives = IntegerVector (n_thresholds);
  false_positives = IntegerVector (n_thresholds);
  true_positives[0] = n_pos;
  false_positives[0] = n_neg;
  tpr = NumericVector(n_thresholds);
  fpr = NumericVector(n_thresholds);
  
  get_positives_delta();
  get_positives();
  get_rate();
  
}

