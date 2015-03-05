\name{all_neighbors}
\alias{all_neighbors}
\docType{package}
\title{
  Test whether a binary predicate holds for all direct neighbors in a list
}
\description{
  Test whether a binary predicate holds for all direct neighbors in a list.
}
\usage{
all_neighbors(f, ...)
}
\arguments{
  \item{f}{
    Binary function to be used to compare elements of `x'.
  }
  \item{...}{
    One or more lists or vectors (holding elements of the same type)..
  }
}
\value{
  TRUE if all neighboring elements (even across lists) compare TRUE,
  otherwise FALSE.
}
\examples{
library(miscFun)

all_neighbors(identical, c(1,1,1))
all_neighbors(identical, c(1,2,1))
all_neighbors(identical, c(NA,NA,NA))
all_neighbors(identical, c(1,NA,1))
all_neighbors(function(x,y) TRUE, 1:5)
all_neighbors(function(x,y) FALSE, c(1,1,1))
all_neighbors(function(x,y) y - x == 1L, 1:10)
all_neighbors(`<`, 1:10, 100:200)
all_neighbors(`<=`, list(a = 1, b = 3), list(c = 3, d = 5))
}
