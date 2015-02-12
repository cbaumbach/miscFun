context("Use template to generate vector for read.table's colClasses argument")

test_that("colClasses can deal with invalid input", {
    expect_that(colClasses("Ncinlrx"), equals(c("NULL", "character", "integer", "numeric", "logical", "raw", "complex")))
    expect_that(colClasses("2 N 2 c 2 i 2 n 2 l 2 r 2 x"), equals(c("NULL", "NULL", "character", "character", "integer", "integer", "numeric", "numeric", "logical", "logical", "raw", "raw", "complex", "complex")))
    expect_that(colClasses("NNcciinnllrrxx"), equals(c("NULL", "NULL", "character", "character", "integer", "integer", "numeric", "numeric", "logical", "logical", "raw", "raw", "complex", "complex")))
    expect_that(colClasses("1N 1c 1i 1n 1l 1r 1x"), equals(c("NULL", "character", "integer", "numeric", "logical", "raw", "complex")))
    expect_that(colClasses("0N 0c 0i 0n 0l 0r x"), equals("complex"))
    expect_that(colClasses(""),   throws_error("fmt must be a non-empty, non-NA character vector of length 1"))
    expect_that(colClasses("  "), throws_error("fmt must be a non-empty, non-NA character vector of length 1"))
    expect_that(colClasses(NA),   throws_error("fmt must be a non-empty, non-NA character vector of length 1"))
})
