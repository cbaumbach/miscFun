\name{group_words}
\alias{group_words}
\docType{package}
\title{
  Group words in chunks of given length
}
\description{
  Group words in chunks of given length
}
\usage{
group_words(x, n, split = "\\\\s+", perl = TRUE, fixed = !perl, sep = " ",
            special = NULL, special_sep = "", trim = TRUE)
}
\arguments{
  \item{x}{
    Vector of strings to be split and regrouped.
  }
  \item{n}{
    Number of characters per chunk of regrouped words.
  }
  \item{split}{
    Pattern on which to split strings in `x' into words.
  }
  \item{perl}{
    TRUE if `split' and `special_sep' should be interpreted as
    perl-style regular expressions.  Takes precedence over `fixed'.
  }
  \item{fixed}{
    TRUE if `split' and `special_sep' should be interpreted as
    fixed strings.
  }
  \item{sep}{
    Separator used when combining words into chunks.
  }
  \item{special}{
    Pattern identifying words that should be combined in a special way
    (see Details).
  }
  \item{special_sep}{
    Separator used to combine two words if the first matches `special'.
  }
  \item{trim}{
    TRUE if whitespace ("\\\\s+") should be remove at start and end of
    chunks.
  }
}
\details{
  The meaning of `word' depends on the value of `split'.  By default,
  strings are split on whitespace resulting in a list of words in the
  normal sense (maybe including some punctuation characters and the
  like).  Clever choice of `split', `sep', `special', and `special_sep'
  let's you obtain a variety of effects (see \code{wrap_lines}).
}
\value{
  A list of character vectors where every character vector contains
  one or more chunks of words.  Every chunk has length <= `n' unless it
  contains a single word of length > `n'.
}
\seealso{
  \code{\link{wrap_lines}}
}
\examples{
library(miscFun)

s <- "The quick yellow-orange-brown fox jumps over the 3-year-old, lazy dog."
group_words(s, 10, split = "\\\\s+|(?<=-)", special = "-$")
}
