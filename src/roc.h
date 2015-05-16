
class ROC{
private:
  void find_thresholds(NumericVector pred, IntegerVector true_class);
  void build_pred(NumericVector pred, IntegerVector true_class);
  IntegerVector build_index(NumericVector pred);
  void get_rate();
  void get_positives_delta();
  void get_positives();
  void reset_delta();
protected:
  NumericVector pred_pos;
  NumericVector pred_neg;
  NumericVector thresholds;
  IntegerVector index_pos;
  IntegerVector index_neg;
  IntegerVector original_index_pos;
  IntegerVector original_index_neg;
  IntegerVector delta_pos;
  IntegerVector delta_neg;
  IntegerVector true_positives;
  IntegerVector false_positives;
  NumericVector tpr;
  NumericVector fpr;
  int n;
  int n_thresholds;
  int n_pos;
  int n_neg;
public:
  ROC(NumericVector pred, IntegerVector true_class);
  void shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg); 
  void strat_shuffle(IntegerVector &shuffle_pos, IntegerVector &shuffle_neg);
  NumericVector get_tpr() const;
  NumericVector get_fpr() const;
  NumericVector get_thresholds() const;
  int get_n_thres() const;
};

