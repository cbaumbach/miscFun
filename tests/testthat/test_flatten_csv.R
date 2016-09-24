context("flatten_csv")

f <- flatten_csv

test_that("happy path", {
    expect_identical(f(c("1", "2,3", "4,5,6")), as.character(1:6))
})

test_that("the separator can be a regular expression", {
    expect_identical(f("1 x 2", sep = "[ x]+", fixed = FALSE), c("1", "2"))
})

test_that("the separator can be the empty string", {
    expect_identical(f("12", sep = ""), c("1", "2"))
})
