context("find_first_match")

test_that("find_first_match works", {

    expect_that(find_first_match(c("a", "b", "c"), c("a", "b")), equals(1L))
    expect_that(find_first_match(c("a", "b", "c"), character()), throws_error("`x' must be a character vector of length >= 1."))
    expect_that(find_first_match(character(), c("a", "b")), throws_error("`candidates' must be a character vector of length >= 1."))
    expect_that(find_first_match(c("x", "y", "z"), c("a", "b")), equals(NA_integer_))
})
