context("colClasses")

f <- colClasses

test_that("happy path", {
    expect_equal(f("Ncinlrx"), c("NULL", "character", "integer", "numeric", "logical", "raw", "complex"))
})

test_that("c", {
    expect_equal(f("c"), "character")
})

test_that("i", {
    expect_equal(f("i"), "integer")
})

test_that("l", {
    expect_equal(f("l"), "logical")
})

test_that("n", {
    expect_equal(f("n"), "numeric")
})

test_that("N", {
    expect_equal(f("N"), "NULL")
})

test_that("r", {
    expect_equal(f("r"), "raw")
})

test_that("x", {
    expect_equal(f("x"), "complex")
})

test_that("we can express repeated elements using counts", {
    expect_equal(f("2x"), f("xx"))
})

test_that("a count of 0 expands to nothing", {
    expect_equal(f("0x"), character())
    expect_equal(f("r0x"), f("r"))
})

test_that("spaces are allowed to improve readability", {
    expect_equal(f("2 r 0 x"), f("2r0x"))
})

test_that("an invalid format throws an error", {
    error_message <- "fmt must be a non-empty, non-NA character vector of length 1"
    for (bad_format in list(character(), "", " ", NA, c("x", "x")))
        expect_error(f(bad_format), error_message)
})

test_that("the first unknown letter in the format throws an error", {
    expect_error(colClasses("cYZ"), "unknown letter in format: Y")
})
