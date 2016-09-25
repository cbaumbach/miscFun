#include <Rcpp.h>
using namespace Rcpp;

//' Find intervals containing a set of points
//'
//' @param pos Double vector of positions
//' @param start Double vector with left endpoints of intervals
//' @param end Double vector with right endpoints of intervals
//'
//' @return
//' A list with elements "position" and "interval".  Both are integer
//' vectors containing indexes.  The "position" vector indexes the
//' \code{pos} vector and the "interval" vector the \code{start} and
//' \code{end} vectors.  The kth index in the "position" vector forms
//' a pair with the kth index in the "interval" vector.  A pair of
//' indexes (i,j) means that \code{pos[i]} belongs to the interval
//' defined by \code{start[j]} and \code{end[j]}.
// [[Rcpp::export]]
List rcpp_find_matching_intervals(DoubleVector pos, DoubleVector start, DoubleVector end) {
    std::vector<int> index_position;
    std::vector<int> index_interval;
    for (int i = 0; i < pos.size(); i++) {
        double position = pos[i];
        for (int j = 0; j < start.size(); j++) {
            double lower = start[j];
            double upper = end[j];
            if (NumericVector::is_na(lower) || NumericVector::is_na(upper))
                continue;
            if (lower <= position && position <= upper) {
                index_position.push_back(i + 1);
                index_interval.push_back(j + 1);
            }
        }
    }
    return List::create(
        Named("position") = as<IntegerVector>(wrap(index_position)),
        Named("interval") = as<IntegerVector>(wrap(index_interval)));
}
