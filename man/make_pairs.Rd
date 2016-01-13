\name{make_ordered_pairs}
\alias{make_ordered_pairs}
\alias{make_unordered_pairs}
\docType{package}
\title{
Create all [un]ordered pairs from two vectors.
}
\description{
Create all [un]ordered pairs from two vectors.
}
\usage{
make_ordered_pairs(x, y = NULL)
make_unordered_pairs(x, y = NULL)
}
\arguments{
\item{x, y}{
Atomic vectors which are paired off.
}
}
\value{
Returns a data frame with column names "x" and "y" where every row
corresponds to a pair.
}
\examples{
make_ordered_pairs(1:3, 1:2)$x == c(1, 2, 3, 1, 2, 3)
make_ordered_pairs(1:3, 1:2)$y == c(1, 1, 1, 2, 2, 2)
make_unordered_pairs(1:3, 1:2)$x == c(1, 2, 3, 2, 3)
make_unordered_pairs(1:3, 1:2)$y == c(1, 1, 1, 2, 2)
}
