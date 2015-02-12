\name{nfields}
\alias{nfields}
\docType{package}
\title{
  Return number of columns in file
}
\description{
  Return number of columns in file.
}
\usage{
nfields(file, sep = "\t")
}
\arguments{
  \item{file}{
    File in which columns should be counted.
  }
  \item{sep}{
    Separator used to separate columns in file.
  }
}
\value{
  Returns number of columns (aka fields) in file.
}
\examples{
library(miscFun)

\dontrun{
nf <- nfields("my_data.csv", sep = ",")
}
}
