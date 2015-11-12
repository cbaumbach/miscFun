context("same_length")

test_that("integer() and double() have the same length", {
    expect_true(same_length(integer(), double()))
})

test_that("NA and 0 have the same length", {
    expect_true(same_length(NA, 0))
})

test_that("NULL has the same length as integer()", {
    expect_true(same_length(NULL, integer()))
})

test_that("three copies of the same vector have the same length", {
    x <- seq_len(3)
    expect_true(same_length(x, x, x))
})

test_that("0, 0:1, and 0:2 have not the same length", {
    expect_false(same_length(0, 0:1, 0:2))
})

test_that("list() has the same length as integer()", {
    expect_true(same_length(list(), integer()))
})
