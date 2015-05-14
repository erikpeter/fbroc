class Bootstrapped_ROC : public ROC{
private:
  Sampler_base *sampler;
public:
  Bootstrapped_ROC(NumericVector pred, IntegerVector true_class);
  ~Bootstrapped_ROC();
  void bootstrap();
};