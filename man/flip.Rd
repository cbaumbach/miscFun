\name{flip}
\alias{flip}
\docType{package}
\title{
  Flip 1st and 2nd argument of a function
}
\description{
  Flip 1st and 2nd argument of a function
}
\usage{
flip(f)
}
\arguments{
  \item{f}{
    Function whose 1st and 2nd argument should be flipped.
  }
}
\value{
  Returns the same function but with its 1st and 2nd argument flipped.
}
\examples{
library(miscFun)

f <- function(x, y, z) x + 2*y + 3*z
g <- flip(f)
f(1, 2, 3) == 14
g(1, 2, 3) == 13
g(z = 3, 1, 2) == 13
}
