\name{maybe}
\alias{maybe}
\docType{package}
\title{
  Maybe apply function to value
}
\description{
  Maybe apply function to value
}
\usage{
maybe(f, x)
}
\arguments{
  \item{f}{
    Function to be applied to `x'.
  }
  \item{x}{
    Object to which `f' should be applied.
  }
}
\value{
  Return `f(x)' unless that results in warnings.  Otherwise return `x'.
}
\examples{
library(miscFun)

x <- "12"
y <- "foo"
maybe(as.integer, x)
maybe(as.integer, y)
}
