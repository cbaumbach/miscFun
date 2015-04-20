\name{gzhead}
\alias{gzhead}
\docType{package}
\title{
  Output the first part of gzip-files
}
\description{
  Output the first part of gzip-files
}
\usage{
gzhead(files, n = 1L, ncore = 1L, simplify = TRUE)
}
\arguments{
  \item{files}{
    Character vector with paths to gzip-files.
  }
  \item{n}{
    Number of lines to print.
  }
  \item{ncore}{
    Number of cores to use in parallel.
  }
  \item{simplify}{
    If TRUE and `n = 1L', return result as character vector.
  }
}
\value{
  In case `simplify = FALSE', returns the first `n' lines of each
  gzip-file in `files' as a list of character vectors.  If `simplify =
  TRUE' and `n = 1' returns the results as a character vector of the
  same length as `files'.
}
\examples{
library(miscFun)

\dontrun{
tmp1 <- tempfile(tmpdir = "/tmp")
tmp2 <- tempfile(tmpdir = "/tmp")
cat("line 1\nline 2\nline 3", file = tmp1)
cat("LINE 1\nLINE 2\nLINE 3", file = tmp2)
gzhead(c(tmp1, tmp2))
file.remove(tmp1, tmp2)
}
}
