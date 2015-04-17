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
maybe(f, x, y = x)
}
\arguments{
  \item{f}{
    Function to be applied to `x'.
  }
  \item{x}{
    Object to which `f' should be applied.
  }
  \item{y}{
    Alternative value to return if applying `f' to `x' leads to an error
    or a warning.
  }
}
\details{
  Errors or warnings caused by evaluating `x' will not be caught.  The
  purpose of \code{maybe} is only to catch errors and warning caused by
  evaluating `f(x)'.
}
\value{
  Return `f(x)' unless that results in an error or a warning.  Otherwise
  return `y'.
}
\examples{
library(miscFun)

x <- "12"
y <- "foo"
maybe(as.integer, x)
maybe(as.integer, y)
maybe(as.integer, y, 0L)
tryCatch(maybe(as.integer, stop(), 0L),
         error = function(e) pr("Evaluating `x' caused an error."))
}
