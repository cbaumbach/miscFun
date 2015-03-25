\name{wrap_lines}
\alias{wrap_lines}
\docType{package}
\title{
  Wrap strings across lines
}
\description{
  Wrap strings across lines
}
\usage{
wrap_lines(x, n, sep = "\\n", max_lines = Inf, dots = "...",
           hard = FALSE, tol = if (hard) 0L else 3L)
}
\arguments{
  \item{x}{
    Strings to be wrapped.
  }
  \item{n}{
    Number of characters per line after wrapping.
  }
  \item{sep}{
    Separator used for combining lines.
  }
  \item{max_lines}{
    Wrap every string to at most `max_lines' lines.
  }
  \item{dots}{
    String to be inserted to indicate that a string was truncated.
  }
  \item{hard}{
    TRUE if lines should have a fixed length of `n' even if that means
    line breaks in the middle of a word.
  }
  \item{tol}{
    Lines will be allowed to be `tol' characters longer than `n' if that
    makes the line lengths more even overall.
  }
}
\value{
  A list of strings each of which spans at most `max_lines' physical
  lines of length 1 to `n + tol'.
}
\seealso{
  \code{\link{group_words}}
}
\examples{
library(miscFun)

s <- "The quick yellow-orange-red-brown fox jumps over a 3-year-old, lazy dog."
pr(wrap_lines(s, 12))
pr(wrap_lines(s, 12, hard = TRUE))
}
