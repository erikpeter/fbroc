typedef double (* PerfFun)(NumericVector &, NumericVector &, NumericVector &);
enum Measure {AUC, TPR_AT_FPR, FPR_AT_TPR};

List roc_analysis(NumericVector pred, IntegerVector true_class);
PerfFun pick_measure(Measure measure);
