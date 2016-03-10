context("match1of")

test_that("match1of works", {

    expect_that(match1of(character(), letters[1:5]), throws_error("`candidates' must be a character vector of length >= 1."))
    expect_that(match1of(letters[1:5], character()), throws_error("`x' must be a character vector of length >= 1."))
    expect_that(match1of(c("a", "b", "c", "d", "e"), c("a", "b", "e")), equals(c("a", "b", "e")))
    expect_that(match1of(c("a", "b", "c", "d", "e"), c("A", "B", "E")), equals(c(NA_character_, NA, NA)))
    expect_that(match1of(c("a", "b", "c", "d", "e"), c(NA_character_, NA)), equals(c(NA_character_, NA)))
})
