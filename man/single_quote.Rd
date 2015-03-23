\name{single_quote}
\alias{single_quote}
\docType{package}
\title{
  Surround string(s) with single quotes
}
\description{
  Surround string(s) with single quotes
}
\usage{
single_quote(x)
}
\arguments{
  \item{x}{
    Character vector.
  }
}
\value{
  Surrounds every element of "x" with single quotes, escaping nested
  single quotes as needed.
}
\examples{
library(miscFun)

single_quote(c("He said \"It's all the same.\"",
               "He said 'It's all the same.'"))
}
