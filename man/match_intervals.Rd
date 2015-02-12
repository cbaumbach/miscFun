\name{match_intervals}
\alias{match_intervals}
\title{
  Match positions to intervals
}
\description{
  Given a vector of positions, return a character vector of the same
  length where every entry consists of a comma-separated string listing
  all intervals that a position falls into.
}
\usage{
match_intervals(pos, start, end, id, batch.size = 1000L)
}
\arguments{
  \item{pos}{
    Integer vector with positions.
  }
  \item{start}{
    Integer vector with interval start positions.
  }
  \item{end}{
    Integer vector with interval end positions.
  }
  \item{id}{
    Vector with interval ids.
  }
  \item{batch.size}{
    Number of positions in 'pos' that are processed at a time.
  }
}
\value{
  A character vector of the same length as 'pos' where every entry is a
  comma-separated string containing the ids of all intervals the
  associated position in 'pos' falls into.  If a position does not fall
  into any of the intervals, the empty string is returned.
}
\examples{
library(miscFun)

pos   <- c(1L,2L,3L)
start <- c(0L,1L,4L)
end   <- c(1L,2L,5L)
id    <- c(1L,2L,3L)
match_intervals(pos, start, end, id)
}
