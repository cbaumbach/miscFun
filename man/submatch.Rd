\name{submatch}
\alias{submatch}
\docType{package}
\title{
  Extract submatches from strings
}
\description{
  Extract submatches from strings
}
\usage{
submatch(pattern, xs, drop = FALSE)
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
  \item{drop}{
    If TRUE instead of returning a single-column or single-row matrix a
    vector is returned.
  }
}
\value{
  Returns a matrix containing submatches extracted from `xs' according
  to `pattern'.  The entry in row i and column j corresponds to the jth
  submatch extracted from `x[i]'.  If `pattern' did not match an element
  of `xs', the corresponding row in the returned matrix will consist
  entirely of NA_character_ values.  If `drop = TRUE' single-column and
  single-row matrices are returned as vectors.
}
\examples{
library(miscFun)

submatch(".*chr(\\\\d+)_(\\\\d+)",
         c("path_chr1_2/to/the_chr10_3_foo.txt",
           "path_chr1_2/to/the_chr7_5_bar.txt",
           "path_chrA_B/to/the_chrX_Y_baz.txt"))
}
