// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_find_matching_intervals
List rcpp_find_matching_intervals(DoubleVector pos, DoubleVector start, DoubleVector end);
RcppExport SEXP miscFun_rcpp_find_matching_intervals(SEXP posSEXP, SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DoubleVector >::type pos(posSEXP);
    Rcpp::traits::input_parameter< DoubleVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< DoubleVector >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_find_matching_intervals(pos, start, end));
    return rcpp_result_gen;
END_RCPP
}
