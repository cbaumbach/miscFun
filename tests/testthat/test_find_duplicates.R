context("Extracting duplicate rows from dataframes.")

test_that("simple cases work as expected", {
    d <- read.table(textConnection("\
x y z
1 2 3
1 3 4
1 2 4
", "r"), header = TRUE)

    expect_that(find_duplicates(d, "x"),              equals(d))
    expect_that(find_duplicates(d, "x", c("y", "z")), equals(d[,c("y", "z")]))
    expect_that(find_duplicates(d, c("x", "y")),      equals(d[c(1L,3L),]))
    expect_that(find_duplicates(d, c("x", "z")),      equals(d[c(2L,3L),]))
})

test_that("NAs are handled correctly", {
    d <- read.table(textConnection("\
x y z
1 2 3
NA 3 4
1 2 4
", "r"), header = TRUE)

    expect_that(find_duplicates(d, "x"),              equals(d[c(1L,3L),]))
    expect_that(find_duplicates(d, "x", c("y", "z")), equals(d[c(1L,3L),c("y", "z")]))
    expect_that(find_duplicates(d, c("x", "y")),      equals(d[c(1L,3L),]))
    expect_that(find_duplicates(d, c("x", "z")),      equals(d[0L,]))
})

test_that("factors are handled correctly", {
    d <- read.table(textConnection("\
x y z
NA 2 a
1 3 b
1 2 b
", "r"), header = TRUE, stringsAsFactors = TRUE)

    expect_that(find_duplicates(d, "x"),              equals(d[c(2L,3L),]))
    expect_that(find_duplicates(d, "x", c("y", "z")), equals(d[c(2L,3L),c("y", "z")]))
    expect_that(find_duplicates(d, c("x", "y")),      equals(d[0L,]))
    expect_that(find_duplicates(d, c("x", "z")),      equals(d[c(2L,3L),]))
})

test_that("character vectors are handled correctly", {
    d <- read.table(textConnection("\
x y z
NA 2 a
1 3 b
1 2 b
", "r"), header = TRUE, stringsAsFactors = FALSE)

    expect_that(find_duplicates(d, "x"),              equals(d[c(2L,3L),]))
    expect_that(find_duplicates(d, "x", c("y", "z")), equals(d[c(2L,3L),c("y", "z")]))
    expect_that(find_duplicates(d, c("x", "y")),      equals(d[0L,]))
    expect_that(find_duplicates(d, c("x", "z")),      equals(d[c(2L,3L),]))
})
