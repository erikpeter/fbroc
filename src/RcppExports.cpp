// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// add_roc_points
List add_roc_points(NumericVector tpr, NumericVector fpr);
RcppExport SEXP fbroc_add_roc_points(SEXP tprSEXP, SEXP fprSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type tpr(tprSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fpr(fprSEXP);
    __result = Rcpp::wrap(add_roc_points(tpr, fpr));
    return __result;
END_RCPP
}
// roc_analysis
List roc_analysis(NumericVector pred, IntegerVector true_class);
RcppExport SEXP fbroc_roc_analysis(SEXP predSEXP, SEXP true_classSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    __result = Rcpp::wrap(roc_analysis(pred, true_class));
    return __result;
END_RCPP
}
// paired_roc_analysis
List paired_roc_analysis(NumericVector pred1, NumericVector pred2, IntegerVector true_class);
RcppExport SEXP fbroc_paired_roc_analysis(SEXP pred1SEXP, SEXP pred2SEXP, SEXP true_classSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred1(pred1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pred2(pred2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    __result = Rcpp::wrap(paired_roc_analysis(pred1, pred2, true_class));
    return __result;
END_RCPP
}
// get_uncached_perf_paired
List get_uncached_perf_paired(NumericVector pred1, NumericVector pred2, IntegerVector true_class, NumericVector param, int n_boot, int measure);
RcppExport SEXP fbroc_get_uncached_perf_paired(SEXP pred1SEXP, SEXP pred2SEXP, SEXP true_classSEXP, SEXP paramSEXP, SEXP n_bootSEXP, SEXP measureSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred1(pred1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pred2(pred2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type param(paramSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type measure(measureSEXP);
    __result = Rcpp::wrap(get_uncached_perf_paired(pred1, pred2, true_class, param, n_boot, measure));
    return __result;
END_RCPP
}
// tpr_at_fpr_delta_uncached
NumericMatrix tpr_at_fpr_delta_uncached(NumericVector pred1, NumericVector pred2, IntegerVector true_class, int n_boot, int n_steps);
RcppExport SEXP fbroc_tpr_at_fpr_delta_uncached(SEXP pred1SEXP, SEXP pred2SEXP, SEXP true_classSEXP, SEXP n_bootSEXP, SEXP n_stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred1(pred1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pred2(pred2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_steps(n_stepsSEXP);
    __result = Rcpp::wrap(tpr_at_fpr_delta_uncached(pred1, pred2, true_class, n_boot, n_steps));
    return __result;
END_RCPP
}
// tpr_fpr_boot2
List tpr_fpr_boot2(NumericVector pred, IntegerVector true_class, int n_boot);
RcppExport SEXP fbroc_tpr_fpr_boot2(SEXP predSEXP, SEXP true_classSEXP, SEXP n_bootSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    __result = Rcpp::wrap(tpr_fpr_boot2(pred, true_class, n_boot));
    return __result;
END_RCPP
}
// get_uncached_perf
NumericVector get_uncached_perf(NumericVector pred, IntegerVector true_class, NumericVector param, int n_boot, int measure);
RcppExport SEXP fbroc_get_uncached_perf(SEXP predSEXP, SEXP true_classSEXP, SEXP paramSEXP, SEXP n_bootSEXP, SEXP measureSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type param(paramSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type measure(measureSEXP);
    __result = Rcpp::wrap(get_uncached_perf(pred, true_class, param, n_boot, measure));
    return __result;
END_RCPP
}
// get_cached_perf
NumericVector get_cached_perf(NumericMatrix tpr, NumericMatrix fpr, NumericVector param, int measure);
RcppExport SEXP fbroc_get_cached_perf(SEXP tprSEXP, SEXP fprSEXP, SEXP paramSEXP, SEXP measureSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type tpr(tprSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type fpr(fprSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type param(paramSEXP);
    Rcpp::traits::input_parameter< int >::type measure(measureSEXP);
    __result = Rcpp::wrap(get_cached_perf(tpr, fpr, param, measure));
    return __result;
END_RCPP
}
// tpr_at_fpr_uncached
NumericMatrix tpr_at_fpr_uncached(NumericVector pred, IntegerVector true_class, int n_boot, int n_steps);
RcppExport SEXP fbroc_tpr_at_fpr_uncached(SEXP predSEXP, SEXP true_classSEXP, SEXP n_bootSEXP, SEXP n_stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    Rcpp::traits::input_parameter< int >::type n_steps(n_stepsSEXP);
    __result = Rcpp::wrap(tpr_at_fpr_uncached(pred, true_class, n_boot, n_steps));
    return __result;
END_RCPP
}
// tpr_at_fpr_cached
NumericMatrix tpr_at_fpr_cached(NumericMatrix tpr, NumericMatrix fpr, int n_thres, int n_steps);
RcppExport SEXP fbroc_tpr_at_fpr_cached(SEXP tprSEXP, SEXP fprSEXP, SEXP n_thresSEXP, SEXP n_stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type tpr(tprSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type fpr(fprSEXP);
    Rcpp::traits::input_parameter< int >::type n_thres(n_thresSEXP);
    Rcpp::traits::input_parameter< int >::type n_steps(n_stepsSEXP);
    __result = Rcpp::wrap(tpr_at_fpr_cached(tpr, fpr, n_thres, n_steps));
    return __result;
END_RCPP
}
// fpr_at_tpr_cached
NumericMatrix fpr_at_tpr_cached(NumericMatrix tpr, NumericMatrix fpr, int n_thres, int n_steps);
RcppExport SEXP fbroc_fpr_at_tpr_cached(SEXP tprSEXP, SEXP fprSEXP, SEXP n_thresSEXP, SEXP n_stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type tpr(tprSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type fpr(fprSEXP);
    Rcpp::traits::input_parameter< int >::type n_thres(n_thresSEXP);
    Rcpp::traits::input_parameter< int >::type n_steps(n_stepsSEXP);
    __result = Rcpp::wrap(fpr_at_tpr_cached(tpr, fpr, n_thres, n_steps));
    return __result;
END_RCPP
}
