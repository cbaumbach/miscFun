\name{one_per_row}
\alias{one_per_row}
\docType{package}
\title{
  One value per row
}
\description{
  In a data frame with a column or columns where entries can consist of
  a comma-separated list of values, split entries with multiple values
  and put them into separate rows.  Eventually there will be only
  "one value per row" in the specified column(s).
}
\usage{
one_per_row(d, cols, sep = ",")
}
\arguments{
  \item{d}{
    Data frame.
  }
  \item{cols}{
    Columns of data frame in which to spread multi-value entries across
    multiple rows.
  }
  \item{sep}{
    Separator used in multi-value entries.
  }
}
\value{
  Returns a data frame with the same columns as `d' but where there is
  only one value per row in the columns `cols'.
}
\examples{
library(miscFun)

d <- read.table(textConnection("\
x y z
1 a,c A,C
2 b,d B,D
3 e E
", "r"), header = TRUE, stringsAsFactors = FALSE)
one_per_row(d, c("y","z"))
}
