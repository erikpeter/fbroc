class ROC{
private:
  void find_thresholds(NumericVector pred, IntegerVector true_class);
  void build_pred(NumericVector pred, IntegerVector true_class);
  IntegerVector* build_index(NumericVector pred);
  NumericVector get_rate(IntegerVector *index) const;
  IntegerVector get_positives_delta(IntegerVector *index) const;
  IntegerVector get_positives(IntegerVector delta, int index_size) const;
protected:
  NumericVector pred_pos;
  NumericVector pred_neg;
  NumericVector thresholds;
  IntegerVector *index_pos;
  IntegerVector *index_neg;
  IntegerVector *new_index_pos;
  IntegerVector *new_index_neg;
  int n;
  int n_thresholds;
  int n_pos;
  int n_neg;
public:
  ROC(NumericVector pred, IntegerVector true_class);
  //void shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg); 
  void strat_shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg);
  NumericVector get_tpr() const;
  NumericVector get_fpr() const;
  NumericVector get_thresholds() const;
  int get_n_thres() const;
};

