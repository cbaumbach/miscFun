\name{rev_map}
\alias{rev_map}
\title{
  Invert a map.
}
\description{
  Given a map implemented as a named list, return the inverse map, again
  implemented as a list.
}
\usage{
rev_map(map)
}
\arguments{
  \item{map}{
    A named list representing a map.
  }
}
\value{
  A list representing the inverse of mapping of `map'.
}
\details{
  NA values are dropped during the inversion.
}
\examples{
library(miscFun)

map <- list(a = c("A","C"), b = c("B","C",NA))
rev_map(map)
}
