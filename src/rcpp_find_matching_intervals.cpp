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

static int get_last_index(int pos, IntegerVector& start)
{
    return std::upper_bound(start.begin(), start.end(), pos)
                    - start.begin();
}

//' Find intervals containing a set of points
//'
//' @param pos Integer vector of positions
//' @param start Integer vector with left endpoints of intervals
//' @param end Integer vector with right endpoints of intervals
//'
//' @return
//' A list with two elements named "position" and "interval".  Both
//' are integer vectors of indexes.  The "position" vector indexes the
//' `pos` argument and the "interval" vector the `start` and `end`
//' arguments.  The kth index in the "position" vector forms a pair
//' with the kth index in the "interval" vector.  A pair of indexes
//' (i,j) means that \code{pos[i]} belongs to the interval defined by
//' \code{start[j]} and \code{end[j]}.
//'
//' @examples
//' rcpp_find_matching_intervals(1:3, 2:3, 4:5)
//'
//' @export
// [[Rcpp::export]]
List rcpp_find_matching_intervals(IntegerVector pos,
        IntegerVector start, IntegerVector end)
{
    std::vector<int> index_position;
    std::vector<int> index_interval;

    for (int i = 0; i < pos.size(); i++) {
        int position = pos[i];
        int last_index = get_last_index(position, start);
        for (int j = 0; j < last_index; j++) {
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
