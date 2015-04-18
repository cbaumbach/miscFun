\name{match1of}
\alias{match1of}
\alias{find_first_match}
\docType{package}
\title{
  Find first matching candidate
}
\description{
  Find first matching candidate
}
\usage{
match1of(candidates, x, \dots)
find_first_match(candidates, x, \dots)
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
  \code{match1of} returns a character vector of the same length as `x'
  where element i contains the first element of `candidates' that
  matches in `x[i]', or NA if none of the candidates match.

  \code{find_first_match} returns the index of the element of
  `candidates' that matches the element of `x' with the smallest index.
  In case of ties, the smallest of the tied indexes is returned.  If
  none of the candidates match any of the elements in `x', NA is
  returned.
}
\examples{
match1of(c("foo", "bar", "xxx", "baz", "quux"), c("foo", "bar", "quux"))
find_first_match(c("foo", "bar", "baz", "quux"), c("baz", "quux", "bar"))
}
