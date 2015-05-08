class Bootstrapped_ROC{
private:
  ROC roc;
  Sampler_base *sampler;
  Shuffled_ROC shuffled;
public:
  Bootstrapped_ROC(NumericVector pred, IntegerVector true_class);
  ~Bootstrapped_ROC();
  void bootstrap();
  NumericVector get_tpr();
  NumericVector get_fpr();
};