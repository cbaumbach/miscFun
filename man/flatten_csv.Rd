\name{flatten_csv}
\alias{flatten_csv}
\docType{package}
\title{Flatten a list of strings of comma-separated elements}
\description{
  Flatten a vector like c("1,2", "3,4,5") into a vector like c("1", "2",
  "3", "4", "5").
}
\usage{
flatten_csv(x, sep = ",", fixed = TRUE)
}
\arguments{
  \item{x}{
    Character vector of strings of separated elements.
  }
  \item{sep}{
    Separator to be used.
  }
  \item{fixed}{
    If TRUE (default), separator is interpreted as a constant string,
    otherwise it is considered a regular expression in perl flavor.
  }
}
\details{
  The separator can also be an arbitrary perl regular expression in
  which case 'fixed' should be set to FALSE.
}
\value{
  A vector like c("1,2", "3,4,5") would be flattened into a vector like
  c("1", "2", "3", "4", "5").
}
\examples{
library(miscFun)

flatten_csv(c("1,2", "3,4,5"))
flatten_csv(c("a1b", "c23d"), sep = "\\\\d+", fixed = FALSE)
}
