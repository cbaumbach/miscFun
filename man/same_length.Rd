\name{same_length}
\alias{same_length}
\docType{package}
\title{
Test whether all arguments have the same length
}
\description{
Test whether all arguments have the same length
}
\usage{
same_length(...)
}
\arguments{
  \item{...}{
    objects whose lengths to compare
  }
}
\value{
  TRUE if all objects have the same length, otherwise FALSE.
}
\examples{
stopifnot(
    same_length(NA, 0),
    same_length(NULL, integer()),
    same_length(list(), integer()),
    same_length(letters, letters, letters),
    !same_length(letters[1], letters[1:2], letters[1:3]))
}
