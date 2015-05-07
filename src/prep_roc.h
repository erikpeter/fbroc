IntegerVector get_tpr_fpr_index(NumericVector pred, IntegerVector true_class, NumericVector thres);
IntegerVector get_class_index(IntegerVector index, IntegerVector true_class,
                              int which_class, int n_class);
NumericVector true_tpr_fpr(NumericVector pred, IntegerVector true_class,
                           NumericVector thres);
IntegerVector find_thresholds(NumericVector pred, IntegerVector true_class);
IntegerVector count_classes(IntegerVector true_class);

