\name{submatch}
\alias{submatch}
\alias{[.submatch}
\alias{print.submatch}
\docType{package}
\title{
  Extract submatches from strings
}
\description{
  Extract submatches from strings
}
\usage{
submatch(pattern, xs)
\method{[}{submatch}(x, i, j, \dots, drop)
\method{print}{submatch}(x, \dots)
}
\arguments{
  \item{pattern}{
    An extended regular expression (see \link[base]{regex}) including at
    least one parenthesized subexpression.
  }
  \item{xs}{
    Character vector with strings from which to extract the
    subexpressions matched by `pattern'.
  }
  \item{x}{
    An object of S3 class "submatch".
  }
  \item{i, j}{
    Indices specifying which submatches to extract.
  }
  \item{drop}{
    If TRUE single-column and single-row matrices are coerced to
    character vectors.  See documentation of \code{\link[base]{[}}.
  }
  \item{\dots}{
    Ignored.
  }
}
\value{
  \code{submatch} returns an object of S3 class "submatch".

  \code{[} return a matrix with submatches or a character vector if
  simplification is possible and `drop = TRUE'.

  \code{print} prints its argument after coercing it to a matrix.
}
\examples{
library(miscFun)

x <- submatch(".*chr(\\\\d+)_(\\\\d+)",
              c("path_chr1_2/to/the_chr10_3_foo.txt",
                "path_chr1_2/to/the_chr7_5_bar.txt",
                "path_chrA_B/to/the_chrX_Y_baz.txt"))

print(x)

x
x[]
x[, ]
x[, 1:2]
x[, 2]
x[, 2, drop = FALSE]
x[1, ]
x[1, , drop = FALSE]
x[3, 1]
x[3, 1, drop = FALSE]
x[1, 1]
x[1, 1, drop = FALSE]
x[1:2, 1:2]
}
