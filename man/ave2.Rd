\name{ave2}
\alias{ave2}
\title{
  Apply function over level combinations of factors
}
\description{
  Group a vector 'x' into groups according to level combinations of the
  factors in 'factors' and apply 'f' to every subset of 'x' separately.
  The function 'ave2' differs from 'ave' in the way arguments are
  specified and, more importantly, in the fact that additional arguments
  to the function 'f' can be specified via '...'.
}
\usage{
ave2(x, factors, f, ...)
}
\arguments{
  \item{x}{
    Vector which will be grouped according to 'factors' and to which 'f'
    will be applied.
  }
  \item{factors}{
    List of factors, each of the same length as 'x', according to whose
    level combinations 'x' will be grouped.
  }
  \item{f}{
    Function that will be applied to grouped subsets of 'x'.
  }
  \item{...}{
    Further arguments to 'f'.
  }
}
\value{
  A vector of the same length as 'x'.  Every subset of 'x' will get the
  value obtained by applying 'f' to it.
}
\seealso{
  ave
}
\examples{
library(miscFun)

fact1 <- rep(1:2, each = 4L)
fact2 <- rep(c("A","B"), length.out = 8L)
x <- 1:8
ave2(x, list(fact1, fact2), function(x, k) k * sum(x), 3)
}
