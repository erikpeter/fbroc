// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// get_tpr_fpr_index
IntegerVector get_tpr_fpr_index(NumericVector pred, IntegerVector true_class, NumericVector thres);
RcppExport SEXP fbroc_get_tpr_fpr_index(SEXP predSEXP, SEXP true_classSEXP, SEXP thresSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thres(thresSEXP);
    __result = Rcpp::wrap(get_tpr_fpr_index(pred, true_class, thres));
    return __result;
END_RCPP
}
// true_tpr_fpr
NumericVector true_tpr_fpr(NumericVector pred, IntegerVector true_class, NumericVector thres);
RcppExport SEXP fbroc_true_tpr_fpr(SEXP predSEXP, SEXP true_classSEXP, SEXP thresSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thres(thresSEXP);
    __result = Rcpp::wrap(true_tpr_fpr(pred, true_class, thres));
    return __result;
END_RCPP
}
// tpr_fpr_boot
NumericMatrix tpr_fpr_boot(NumericVector pred, IntegerVector true_class, NumericVector thres, int n_boot);
RcppExport SEXP fbroc_tpr_fpr_boot(SEXP predSEXP, SEXP true_classSEXP, SEXP thresSEXP, SEXP n_bootSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< int >::type n_boot(n_bootSEXP);
    __result = Rcpp::wrap(tpr_fpr_boot(pred, true_class, thres, n_boot));
    return __result;
END_RCPP
}
// get_auc
NumericVector get_auc(NumericMatrix tpr_fpr);
RcppExport SEXP fbroc_get_auc(SEXP tpr_fprSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type tpr_fpr(tpr_fprSEXP);
    __result = Rcpp::wrap(get_auc(tpr_fpr));
    return __result;
END_RCPP
}
// get_tpr_matrix
NumericVector get_tpr_matrix(NumericMatrix tpr_fpr, int n_steps);
RcppExport SEXP fbroc_get_tpr_matrix(SEXP tpr_fprSEXP, SEXP n_stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type tpr_fpr(tpr_fprSEXP);
    Rcpp::traits::input_parameter< int >::type n_steps(n_stepsSEXP);
    __result = Rcpp::wrap(get_tpr_matrix(tpr_fpr, n_steps));
    return __result;
END_RCPP
}
// find_thresholds
IntegerVector find_thresholds(NumericVector pred, IntegerVector true_class);
RcppExport SEXP fbroc_find_thresholds(SEXP predSEXP, SEXP true_classSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type pred(predSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type true_class(true_classSEXP);
    __result = Rcpp::wrap(find_thresholds(pred, true_class));
    return __result;
END_RCPP
}
