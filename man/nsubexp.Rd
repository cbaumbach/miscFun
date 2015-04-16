\name{nsubexp}
\alias{nsubexp}
\docType{package}
\title{
  Count number of parenthesize subexpressions in string
}
\description{
  Count number of parenthesize subexpressions in string
}
\usage{
nsubexp(pattern)
}
\arguments{
  \item{pattern}{
    Regular expression.
  }
}
\value{
  Returns the number of parenthesized subexpressions in `pattern'.
}
\examples{
library(miscFun)

nsubexp("a_[(string)]_with_(2)_parenthesized_(sub)expressions")
}
