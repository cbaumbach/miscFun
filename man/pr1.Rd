\name{pr1}
\alias{pr1}
\docType{package}
\title{
  Print message without newline
}
\description{
  Print message without newline
}
\usage{
pr1(...)
}
\arguments{
  \item{...}{
    Any number of arguments that could also be handed to cat.
  }
}
\value{
  Prints arguments pasted together (without newline)
}
\examples{
library(miscFun)

pr1("Finished iteration ", 3, " of ", 10, "\n")
}
