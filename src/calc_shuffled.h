NumericVector build_tpr_fpr(IntegerVector tpr_fpr_index, int n_pos, 
                          int n_neg, int n_thres);
NumericMatrix tpr_fpr_boot_iterate(int n_thres, 
                                   IntegerVector tpr_fpr_index,
                                   IntegerVector pos_index,
                                   IntegerVector neg_index);
IntegerVector get_boot_tpr_fpr_index(IntegerVector pos_index, 
                                     IntegerVector neg_index);                                   