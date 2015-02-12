\name{exit}
\alias{exit}
\docType{package}
\title{
  Exit from running R job
}
\description{
  Exit from running R job returning exit status.
}
\usage{
exit(status = 0L, ...)
}
\arguments{
  \item{status}{
    Exit status.
  }
  \item{...}{
    Other arguments that could also be passed to quit.
  }
}
\value{
  None.
}
\examples{
library(miscFun)

\dontrun{
exit(1L)
}
}
