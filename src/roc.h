class ROC{
private:
  void find_thresholds(NumericVector pred, IntegerVector true_class);
  void build_pred(NumericVector pred, IntegerVector true_class);
  IntegerVector build_index(NumericVector pred);
  NumericVector get_rate(IntegerVector index) const;
  IntegerVector get_positives_delta(IntegerVector index) const;
  IntegerVector get_positives(IntegerVector delta, int index_size) const;
protected:
  NumericVector pred_pos;
  NumericVector pred_neg;
  NumericVector thresholds;
  IntegerVector index_pos;
  IntegerVector index_neg;
  int n;
  int n_thresholds;
  int n_pos;
  int n_neg;
public:
  ROC(NumericVector pred, IntegerVector true_class);
  NumericVector get_tpr() const;
  NumericVector get_fpr() const;
  NumericVector get_thresholds() const;
  ROC();
  ROC(const ROC &other);
};

class Shuffled_ROC : public ROC{
  public:
  Shuffled_ROC();
  Shuffled_ROC(const ROC &roc, 
               IntegerVector shuffle_pos, 
               IntegerVector shuffle_neg);
};