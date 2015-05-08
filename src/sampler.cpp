#include <Rcpp.h>
using namespace Rcpp;
#include "sampler.h"


IntegerVector Sampler_base::get_shuffled_index(bool which_class) const
{
  if (which_class) return shuffled_pos_index;
  else return shuffled_neg_index;
}

IntegerVector Sampler_Stratified::get_class_index(IntegerVector true_class, 
                                                  bool which_class) const
{
  int n_class;
  if (which_class) n_class = n_pos;
  else n_class = n_neg;
  IntegerVector class_index (n_class);
  int j = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == which_class) class_index[j++] = i;
  }
  return class_index;
}

Sampler_Stratified::Sampler_Stratified(IntegerVector true_class)
{
  n = true_class.size();
  n_pos = 0;
  n_neg = 0;
  for (int i = 0; i < n; i++) {
    if (true_class[i] == 1) n_pos++;
    else n_neg++;
  }
  pos_index = get_class_index(true_class, true);
  neg_index = get_class_index(true_class, false);
}

void Sampler_Stratified::generate()
{
  // call R for random number generation
  NumericVector random_pos = runif(n_pos);
  NumericVector random_neg = runif(n_neg);
  
  // use stratified bootstrapping
  for (int i = 0; i < n_pos; i++) {
    int random_index = (int)(n_pos * random_pos[i]);
    shuffled_pos_index[i] = pos_index[random_index];
  }
  for (int i = 0; i < n_neg; i++) {
    int random_index = (int)(n_neg * random_neg[i]);
    shuffled_neg_index[i] = neg_index[random_index];
  }
}