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
            special = NULL, special_sep = "", trim_leading = TRUE,
            trim_trailing = trim_leading, tol = 0L, punct = TRUE)
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
  \item{trim_leading}{
    If "TRUE", leading whitespace will be removed from chunks.
  }
  \item{trim_trailing}{
    If "TRUE", trailing whitespace will be removed from chunks.
  }
  \item{tol}{
    Chunks will be allowed to be `tol' characters longer than `n' if
    that makes their lengths more even in overall.
  }
  \item{punct}{
    If "TRUE", words consisting of a single punctuation mark will always
    be merged to the previous chunk.
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
  one or more chunks of words.  Every chunk contains from 1 to `n + tol'
  characters unless it contains a single word of length > `n' in which
  case its length is the length of the word.
}
\seealso{
  \code{\link{wrap_lines}}
}
\examples{
library(miscFun)

s <- "The quick yellow-orange-brown fox jumps over the 3-year-old, lazy dog."
group_words(s, 10, split = "\\\\s+|(?<=-)", special = "-$")
}
