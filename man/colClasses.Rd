\name{colClasses}
\alias{colClasses}
\docType{package}
\title{
  Expand template into read.table's colClasses argument
}
\description{
  Expand template into read.table's colClasses argument.
}
\usage{
colClasses(fmt)
}
\arguments{
  \item{fmt}{
    Format string to be expanded and then used as the colClasses
    argument to read.table.
  }
}
\value{
  Returns a vector that can be used as the colClasses argument to
  read.table.
}
\details{
  The format allows digits and the letters N, c, i, and n which will
  expand into "NULL", "character", "integer", and "numeric".  The digits
  say how many times the expansion of the character should be included
  in the result.  For example, colClasses(c("2N3cin")) will produce a
  character vector with 2+3+1+1=7 elements: "NULL", "NULL", "character",
  "character", "character", "integer", and "numeric".
}
\examples{
library(miscFun)

colClasses("3N2cin14Ni")
}
