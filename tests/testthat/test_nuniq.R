context("nuniq")

test_that("happy path", {
    expect_equal(nuniq(c(1, 2, 1)), 2)
    expect_equal(nuniq(list(1, 2, 1)), 2)
})

test_that("an empyt vector/list contains 0 elements", {
    expect_equal(nuniq(integer()), 0)
    expect_equal(nuniq(list()), 0)
})

test_that("NULL contains 0 elements", {
    expect_equal(nuniq(NULL), 0)
})

test_that("NA counts as a unique value", {
    expect_equal(nuniq(c(1, NA)), 2)
})

test_that("subtypes of NA count as the same value", {
    expect_equal(nuniq(list(NA, NA_integer_, NA_real_, NA_character_, NA_complex_)), 5)
})

test_that("a factor has as many unique elements as levels", {
    expect_equal(nuniq(factor(1:3)), 3)
})
