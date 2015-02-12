\name{pr}
\alias{pr}
\docType{package}
\title{
  Print message followed by newline
}
\description{
  Print message followed by newline.
}
\usage{
pr(...)
}
\arguments{
  \item{...}{
    Any number of arguments that could also be handed to cat.
  }
}
\value{
  Prints arguments pasted together and followed by newline.
}
\examples{
library(miscFun)

pr("Finished iteration ", 3, " of ", 10)
}
