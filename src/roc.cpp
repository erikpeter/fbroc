#include <Rcpp.h>
using namespace Rcpp;

#include "roc.h"


void ROC::strat_shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg) {
  IntegerVector *temp = index_pos;
  IntegerVector *temp2 = index_neg;
  
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
}
/*
void ROC::shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg) {
  n_pos = shuffle_pos.size();
  n_neg = shuffle_neg.size();
  IntegerVector new_index_pos(n_pos);
  IntegerVector new_index_neg(n_neg);
  for (int i = 0; i < n_pos; i++) new_index_pos[i] = index_pos[shuffle_pos[i]];
  for (int i = 0; i < n_neg; i++) new_index_neg[i] = index_neg[shuffle_neg[i]];
  index_pos = new_index_pos;
  index_neg = new_index_neg;
}
*/
IntegerVector ROC::get_positives(IntegerVector delta, int index_size) const {
  IntegerVector positives (n_thresholds);
  positives[0] = index_size;
  for (int i = 1; i < n_thresholds; i++) {
    positives[i] = positives[i - 1] - delta[i];
  }
  return positives;
}
IntegerVector ROC::get_positives_delta(IntegerVector *index) const
{
  IntegerVector delta (n_thresholds);
  for (int i = 0; i < index->size(); i++) {
    delta[index[i]] = delta[index[i]] + 1;
  }
  return delta;
}

NumericVector ROC::get_rate(IntegerVector *index) const
{
  NumericVector out(n_thresholds);
  
  double multiplier = 1. / index->size();
  IntegerVector delta = get_positives_delta(index);
  IntegerVector positives = get_positives(delta, index->size());
  
  for (int i = 0; i < n_thresholds; i++) {
    out[i] = multiplier * positives[i];
  }
  
  return out;
}

NumericVector ROC::get_thresholds() const 
{
  return thresholds;
}

NumericVector ROC::get_tpr() const
{
  NumericVector out = get_rate(index_pos);
  return out;
}

NumericVector ROC::get_fpr() const
{
  NumericVector out = get_rate(index_neg);
  return out;
}


IntegerVector* ROC::build_index(NumericVector pred)
{
  IntegerVector* index = new IntegerVector(pred.size());
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
}

