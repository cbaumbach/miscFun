\name{sort_intervals}
\alias{sort_intervals}
\docType{package}
\title{
Sort intervals in increasing order
}
\description{
Sort intervals in increasing order
}
\usage{
sort_intervals(start, end)
}
\arguments{
  \item{start}{
    left endpoints of intervals
  }
  \item{end}{
    right endpoints of intervals
  }
}
\value{
  A list with two vectors named "start" and "end" of the same length
  as `start` and `end` and representing the left and right endpoints
  of the sorted intervals.
}
\examples{
sort_intervals(2:1, 3:4)
}
