\name{pr}
\alias{pr}
\alias{pr1}
\docType{package}
\title{
  Print message
}
\description{
  Print a message: \code{pr} automatically add a newline at the end
  while \code{pr1} doesn't.
}
\usage{
pr(...)
pr1(...)
}
\arguments{
  \item{...}{
    Any number of arguments that could also be handed to cat.
  }
}
\value{
  Prints arguments pasted together and, in case of \code{pr}, followed
  by newline.
}
\seealso{
  \code{\link[base]{cat}}
  \code{\link[base]{writeLines}}
}
\examples{
library(miscFun)

pr("Finished iteration ", 3, " out of ", 10)
pr1("Finished iteration ", 3, " out of ")
pr(10)
}
