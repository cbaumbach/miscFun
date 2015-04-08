\name{is.directory}
\alias{is.directory}
\docType{package}
\title{
  Test for directory
}
\description{
  Test for directory
}
\usage{
is.directory(fs)
}
\arguments{
  \item{fs}{
    Filenames to be tested for directory status.
  }
}
\value{
  Returns "TRUE" for directories, otherwise "FALSE".
}
\examples{
library(miscFun)

is.directory(".")
}
