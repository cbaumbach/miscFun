#include <Rcpp.h>
using namespace Rcpp;

static bool is_na_interval(int start, int end)
{
    return (start == NA_INTEGER) || (end == NA_INTEGER);
}

static bool belongs_to_interval(int pos, int start, int end)
{
    return (start <= pos) && (pos <= end);
}

// [[Rcpp::export]]
List rcpp_find_matching_intervals(IntegerVector pos,
        IntegerVector start, IntegerVector end)
{
    std::vector<int> index_position;
    std::vector<int> index_interval;

    for (int i = 0; i < pos.size(); i++) {
        int position = pos[i];
        for (int j = 0; j < start.size(); j++) {
            if (is_na_interval(start[i], end[i]))
                continue;
            if (belongs_to_interval(position, start[j], end[j])) {
                index_position.push_back(i + 1);
                index_interval.push_back(j + 1);
            }
        }
    }

    return List::create(
        Named("position") = as<IntegerVector>(wrap(index_position)),
        Named("interval") = as<IntegerVector>(wrap(index_interval)));
}
