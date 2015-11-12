\name{order_intervals}
\alias{order_intervals}
\docType{package}
\title{
Ordering permutation for sorting intervals in increasing order
}
\description{
Ordering permutation for sorting intervals in increasing order
}
\usage{
order_intervals(start, end)
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
  An integer vector \code{x} which, when used as an index to will sort
  `start` and `end` into increasing order just as if
  \code{\link[miscFun]{sort_intervals}} was used.
}
\seealso{
\code{\link[miscFun]{sort_intervals}}
}
\examples{
start <- 2:1
end <- 3:4
ord <- order_intervals(start, end)
start_sorted <- start[ord]
end_sorted <- end[ord]
old_order <- order(ord)
stopifnot(all(start_sorted[old_order] == start))
stopifnot(all(end_sorted[old_order] == end))
}
