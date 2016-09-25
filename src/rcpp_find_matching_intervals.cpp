#include <Rcpp.h>
using namespace Rcpp;

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
