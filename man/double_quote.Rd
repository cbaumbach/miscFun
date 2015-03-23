\name{double_quote}
\alias{double_quote}
\docType{package}
\title{
  Surround string(s) with double quotes
}
\description{
  Surround string(s) with double quotes
}
\usage{
double_quote(x)
}
\arguments{
  \item{x}{
    Character vector.
  }
}
\value{
  Surrounds every element of "x" with double quotes, escaping nested
  double quotes as needed.
}
\examples{
library(miscFun)

double_quote(c("He said \"It's all the same.\"",
               "He said 'It's all the same.'"))
}
