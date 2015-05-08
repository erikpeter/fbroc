class Sampler_base {
protected:
  int n_pos;
  int n_neg;
  int n;
  IntegerVector pos_index;
  IntegerVector neg_index;
  IntegerVector shuffled_pos_index;
  IntegerVector shuffled_neg_index;
public:
  virtual void generate() = 0;
  IntegerVector get_shuffled_index(bool which_class) const;
};

class Sampler_Stratified : public Sampler_base {
private:
  IntegerVector get_class_index(IntegerVector true_class, bool which_class) const;
public:
  virtual void generate();
  Sampler_Stratified(IntegerVector true_class);
};