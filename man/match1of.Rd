\name{match1of}
\alias{match1of}
\docType{package}
\title{
  Find first matching candidate
}
\description{
  Find first matching candidate
}
\usage{
match1of(candidates, x, \dots)
}
\arguments{
  \item{candidates}{
    Character vector of candidates to be matched against `x'.
  }
  \item{x}{
    Character vector.
  }
  \item{\dots}{
    Further arguments passed on to \code{\link[base]{grepl}}.
  }
}
\value{
  A character vector of the same length as `x' where element i contains
  the first element of `candidates' that matches in `x[i]', or NA if
  none of the candidates match.
}
\examples{
library(miscFun)

match1of(c("foo", "bar", "xxx", "baz", "quux"), c("foo", "bar", "quux"))
}
