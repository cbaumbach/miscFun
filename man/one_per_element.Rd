\name{one_per_element}
\alias{one_per_element}
\docType{package}
\title{
  One value per vector element
}
\description{
  Split comma-separated values in a character vector across multiple
  indexes such that eventually there is "one value per element".
}
\usage{
one_per_element(x, sep = ",", perl = FALSE)
}
\arguments{
  \item{x}{
    Character vector.
  }
  \item{sep}{
    Separator used in multi-value vector elements.
  }
  \item{perl}{
    Consider `sep' as a perl regular expression.  If FALSE (default),
    `sep' is considered a literal (fixed) string.
  }
}
\value{
  Returns a character vector where every element contains exactly one
  string.  Empty strings in `x' will not be part of the return vector.
}
\examples{
library(miscFun)

x <- c("1,2", "3,4,5", "6", "", "7,8")
one_per_element(x)

y <- c("1---2", "3--4----5", "6-7")
one_per_element(y, sep = "-+", perl = TRUE)
}
