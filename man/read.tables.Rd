\name{read.tables}
\alias{read.tables}
\docType{package}
\title{
  Read and concatenate tables
}
\description{
  Read and concatenate tables
}
\usage{
read.tables(files, sep = NULL, ncore = 1L, header = TRUE,
            colClasses = "character", verbose = TRUE,
            check.names = FALSE, comment.char = "#", \dots)
}
\arguments{
  \item{files}{
    Files with tables to be read and concatenated.
  }
  \item{sep}{
    Field separator in input tables.
  }
  \item{ncore}{
    Number of cores to use in parallel.
  }
  \item{header}{
    See documentation of \code{\link[utils]{read.table}}.
  }
  \item{colClasses}{
    See documentation of \code{\link[utils]{read.table}}.
  }
  \item{verbose}{
    If TRUE, report progress while running.
  }
  \item{check.names}{
    See documentation of \code{\link[utils]{read.table}}.
  }
  \item{comment.char}{
    See documentation of \code{\link[utils]{read.table}}.
  }
  \item{\dots}{
    Passed on to \code{\link[utils]{read.table}}.
  }
}
\details{
  If not explicitly specified, the field separator will be inferred from
  the first non-comment, non-empty line of one of the input files.
  Possible candidates are tab, comma, semicolon, single space.  They
  will be tested in the above order.  The first candidate found will
  become the field separator. The field separator must be the same for
  all input files.
}
\value{
  Returns the data frame that results from concatenating all input
  tables.
}
\examples{
library(miscFun)

\dontrun{
read.tables(paste0("table", 1:100, ".txt"), ncore = 10L)
}
}
