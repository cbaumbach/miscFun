\name{nuniq}
\alias{nuniq}
\docType{package}
\title{
  Return number of unique elements
}
\description{
  Return number of unique elements.
}
\usage{
nuniq(x)
}
\arguments{
  \item{x}{
    List or vector.
  }
}
\value{
  Returns the number of unique elements in x.
}
\examples{
library(miscFun)

nuniq(rep(LETTERS[1:5], 1:5))
}
