\name{find_duplicates}
\alias{find_duplicates}
\title{
  Extract duplicated rows from data frame.
}
\description{
  Extract all rows from a data frame that are duplicates with respect to
  the specified columns.  Two rows are duplicates with respect to the
  selected columns if they agree element-wise in all of the selected
  columns.
}
\usage{
find_duplicates(d, cols, select = colnames(d), sep = "\t")
}
\arguments{
  \item{d}{
    Data frame.
  }
  \item{cols}{
    Columns in `d' to be checked for duplicates.  Can be a character
    vector of column names or a numeric vector giving the column
    indexes.
  }
  \item{select}{
    Columns to include in resulting data frame.
  }
  \item{sep}{
    Separator used for row-wise pasting of values in `cols' columns.
  }
}
\value{
  A data frame containing all rows of `d' that had duplicates with
  respect to the selected columns.
}
\seealso{
  duplicated
}
\examples{
library(miscFun)

d <- read.table(textConnection("\
x y z
1 2 3
1 3 4
1 2 4
", "r"), header = TRUE)

find_duplicates(d, "x")
find_duplicates(d, "x", c("y", "z"))
find_duplicates(d, c("x", "y"))
find_duplicates(d, c("x", "z"))
}
