context("nuniq")

test_that("nuniq handles missings and factors correctly", {
    expect_that(nuniq(NA),                                      equals(1L))
    expect_that(nuniq(c(NA_integer_, NA_real_, NA_character_)), equals(1L))
    expect_that(nuniq(factor(letters[1:3])),                    equals(3L))
})

test_that("nuniq handles non-vector lists correctly", {
    expect_that(nuniq(list(1, 1)),                                 equals(1L))
    expect_that(nuniq(list(NA_integer_, NA_real_, NA_character_)), equals(3L))
    expect_that(nuniq(list(1:2, 1:2)),                             equals(1L))
})
