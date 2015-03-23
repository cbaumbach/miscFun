\name{miscFun-package}
\alias{miscFun-package}
\alias{miscFun}
\docType{package}
\title{
Miscellaneous functions
}
\description{
This is a collection of functions which, if not for this package, I
would have to rewrite or copy/paste for almost every R script that I
write.
}
\details{
\tabular{ll}{
Package: \tab miscFun\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-12\cr
Suggests: \tab testthat\cr
License: \tab GPL (>= 2)\cr
Packaged: \tab 2015-02-13 20:23:00 UTC; cb\cr
Built: \tab R 3.1.1; ; 2015-02-13 20:24:05 UTC; unix\cr
}

Index:
\preformatted{
all_neighbors    Test whether binary predicate holds list elements
ave2             Apply function over level combinations of factors
colClasses       Expand template into read.table's colClasses argument
exit             Exit from running R job
find_duplicates  Extract duplicated rows from data frame
flatten_csv      Flatten a list of strings of comma-separated elements
match_intervals  Match positions to intervals
nfields          Return number of columns in file
nuniq            Return number of unique elements
one_per_element  One value per vector element
one_per_row      One value per row
pr               Print message followed by newline
pr1              Print message without newline
rename           Change names of named list
rev_map          Invert a map
}
}
\author{
Clemens Baumbach

Maintainer: Clemens Baumbach <clemens.baumbach@helmholtz-muenchen.de>
}
\references{
none
}
\keyword{ manip }
\keyword{ attribute }
\keyword{ list }
\keyword{ print }
\keyword{ utilities }
\keyword{ package }
\seealso{
  \code{\link[stats]{ave}},
  \code{\link[plyr]{rename}},
  \code{\link[reshape]{rename}},
  \code{\link[base]{cat}},
  \code{\link[base]{quit}}
}
\examples{
library(miscFun)

## all_neighbors
all_neighbors(identical, c(1,1,1))
all_neighbors(identical, c(1,2,1))
all_neighbors(identical, c(NA,NA,NA))
all_neighbors(identical, c(1,NA,1))
all_neighbors(function(x,y) TRUE, 1:5)
all_neighbors(function(x,y) FALSE, c(1,1,1))
all_neighbors(function(x,y) y - x == 1L, 1:10)
all_neighbors(`<`, 1:10, 100:200)
all_neighbors(`<=`, list(a = 1, b = 3), list(c = 3, d = 5))

## ave2
fact1 <- rep(1:2, each = 4L)
fact2 <- rep(c("A","B"), length.out = 8L)
x <- 1:8
ave2(x, list(fact1, fact2), function(x, k) k * sum(x), 3)

## colClasses
colClasses("3N2cin14Ni")

## exit
\dontrun{
exit(1L)
}

## find_duplicated
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

## flatten_csv
flatten_csv(c("1,2", "3,4,5"))
flatten_csv(c("a1b", "c23d"), sep = "\\\\d+", fixed = FALSE)

## match_intervals
pos   <- c(1L,2L,3L)
start <- c(0L,1L,4L)
end   <- c(1L,2L,5L)
id    <- c(1L,2L,3L)
match_intervals(pos, start, end, id)

## nfields
\dontrun{
nf <- nfields("my_data.csv", sep = ",")
}

## nuniq
nuniq(rep(LETTERS[1:5], 1:5))

## one_per_element
x <- c("1,2", "3,4,5", "6")
one_per_element(x)

y <- c("1---2", "3--4----5", "6-7")
one_per_element(y, sep = "-+", perl = TRUE)

## one_per_row
d <- read.table(textConnection("\
x y z
1 a,c A,C
2 b,d B,D
3 e E
", "r"), header = TRUE, stringsAsFactors = FALSE)
one_per_row(d, c("y","z"))

## pr
pr("Finished iteration ", 3, " of ", 10)

## pr1
pr1("Finished iteration ", 3, " of ", 10, "\n")

## rename
x <- c(a = 1, b = 2, c = 3, `9` = 9)
old2new <- c(a = "A", b = "B", c = NA, g = "G", a = "X", `9` = "nine")
x <- rename(x, old2new)

## rev_map
map <- list(a = c("A","C"), b = c("B","C",NA))
rev_map(map)
}
