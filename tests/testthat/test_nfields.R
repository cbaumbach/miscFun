context("nfields")

filename <- "data/tmp.txt"

test_that("happy path", {
    cat("A", "B", "C", sep = "\t", file = filename)
    expect_equal(nfields(filename), 3)
})

test_that("we can select the field separator", {
    cat("A", "B", "C", sep = " ", file = filename)
    expect_equal(nfields(filename, sep = " "), 3)
})

test_that("instead of a filename we can also pass a connection", {
    con <- textConnection("A B C")
    expect_equal(nfields(con, sep = " "), 3)
})

file.remove(filename)
