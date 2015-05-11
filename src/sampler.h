class Sampler_base {
protected:
  int n_pos;
  int n_neg;
  int n;
  IntegerVector shuffled_pos_index;
  IntegerVector shuffled_neg_index;
public:
  virtual void generate() = 0;
  IntegerVector get_shuffled_index(bool which_class) const;
};

class Sampler_Stratified : public Sampler_base {
public:
  virtual void generate();
  Sampler_Stratified(IntegerVector true_class);
};