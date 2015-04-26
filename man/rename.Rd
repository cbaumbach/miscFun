\name{rename}
\alias{rename}
\title{
  Return modified names of object
}
\description{
  Return modified names of object
}
\usage{
rename(x, old2new, warn = TRUE)
}
\arguments{
  \item{x}{
    Object whose `names' should be modified.
  }
  \item{old2new}{
    Named character vector whose name tags correspond to the old names
    of `x' and whose elements correspond to the new names of `x'.
  }
  \item{warn}{
    If TRUE, warn about stuff (see Details).
  }
}
\value{
  Return a character vector that can be used to change the `names'
  attribute of `x' in place using \code{\link[base]{names<-}}.
}
\details{
  The \code{rename} function does not modify `x' and thereby avoids
  potentially expensive copies due to R's copy-on-modify semantics.  If
  you want to change the `names' attribute of `x' in a non-expensive
  way, use \code{\link[base]{names<-}} with `x' and the result of the
  call to \code{rename}.

  Note that `old2new' does not have to contain new values for all
  elements of `names(x)'.  If an element of `names(x)' is not among the
  tags in `old2new', it will be unchanged.  If `old2new' contains the
  same tag more than once, the first element with the given tag will be
  used for changing the matching element in `names(x)'. If `old2new'
  contains a mixture of tagged and untagged elements, the untagged
  elements are implicitly tagged by the emptystring ("").  If `old2new'
  has no `names' attribute, the unmodified `names' attribute of `x' is
  returned.

  Unless `warn = FALSE' \code{rename} will issue a warning if
  \enumerate{
    \item `old2new' contains non-empty (non-"") tags that do not match
          any element of `names(x)';
    \item the `names' attribute of `old2new' is empty (NULL);
    \item `names(x)' contains duplicates after renaming.
  }
}
\examples{
library(miscFun)

x <- c(a = 1, b = 2, c = 3, `9` = 9)
old2new <- c(a = "A", b = "B", c = NA, "foo", g = "G", a = "X", `9` = "nine")
names(x) <- rename(x, old2new)
}
