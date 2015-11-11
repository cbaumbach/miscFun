\name{count_points_per_interval}
\alias{count_points_per_interval}
\docType{package}
\title{
Count how many point fall into each interval
}
\description{
Count how many point fall into each interval
}
\usage{
count_points_per_interval(pos, start, end)
}
\arguments{
  \item{pos}{
    points
  }
  \item{start}{
    left endpoints of intervals
  }
  \item{end}{
    right endpoints of intervals
  }
}
\value{
  An integer vector of the same length as `start` and `end` containing
  the number of points in `pos` that fall into each of the intervals.
}
\examples{
count_points_per_interval(0:1, 2:1, 3:4)
}
