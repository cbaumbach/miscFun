\name{rcpp_find_matching_intervals}
\alias{rcpp_find_matching_intervals}
\docType{package}
\title{
Find intervals containing a set of points
}
\description{
Find intervals containing a set of points
}
\usage{
rcpp_find_matching_intervals(pos, start, end)
}
\arguments{
  \item{pos}{
    integer vector of positions
  }
  \item{start}{
    integer vector with left endpoints of intervals
  }
  \item{end}{
    integer vector with right endpoints of intervals
  }
}
\value{
  A list with two elements named "position" and "interval".  Both
  are integer vectors of indexes.  The "position" vector indexes the
  `pos` argument and the "interval" vector the `start` and `end`
  arguments.  The kth index in the "position" vector forms a pair with
  the kth index in the "interval" vector.  A pair of indexes (i,j)
  means that \code{pos[i]} belongs to the interval defined by
  \code{start[j]} and \code{end[j]}.
}
\examples{
rcpp_find_matching_intervals(1:3, 2:3, 4:5)
}
