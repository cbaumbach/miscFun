context("sort_intervals")

f <- sort_intervals

test_that("the components of the returned list are called 'start' and 'end'", {
    actual <- names(f(0, 0))
    expected <- c("start", "end")
    expect_equal(actual, expected)
})

test_that("[0,1] < [2,3]", {
    actual <- f(start = c(2, 0), end = c(3, 1))
    expected <- list(start = c(0, 2), end = c(1, 3))
    expect_equal(actual, expected)
})

test_that("[0,2] < [1,3]", {
    actual <- f(start = c(1, 0), end = c(3, 2))
    expected <- list(start = c(0, 1), end = c(2, 3))
    expect_equal(actual, expected)
})

test_that("[0,1] < [0,2]", {
    actual <- f(start = c(0, 0), end = c(2, 1))
    expected <- list(start = c(0, 0), end = c(1, 2))
    expect_equal(actual, expected)
})

test_that("[0,3] < [1,2]", {
    actual <- f(start = c(1, 0), end = c(2, 3))
    expected <- list(start = c(0, 1), end = c(3, 2))
    expect_equal(actual, expected)
})

test_that("[0,1] == [0,1]", {
    actual <- f(start = c(0, 0), end = c(1, 1))
    expected <- list(start = c(0, 0), end = c(1, 1))
    expect_equal(actual, expected)
})

test_that("[0,1] < [2,3] < [4,5]", {
    actual <- f(start = c(2, 4, 0), end = c(3, 5, 1))
    expected <- list(start = c(0, 2, 4), end = c(1, 3, 5))
    expect_equal(actual, expected)
})

test_that("[0,1] < [0,2] < [0,3]", {
    actual <- f(start = c(0, 0, 0), end = c(2, 1, 3))
    expected <- list(start = c(0, 0, 0), end = c(1, 2, 3))
    expect_equal(actual, expected)
})

test_that("[0,1] < [0,5] < [2,3] < [2,6]", {
    actual <- f(start = c(2, 0, 2, 0), end = c(3, 1, 6, 5))
    expected <- list(start = c(0, 0, 2, 2), end = c(1, 5, 3, 6))
    expect_equal(actual, expected)
})

test_that("[1,2] < [0,NA] < [NA,0] < [NA,NA]", {
    actual <- f(start = c(NA, NA, 0, 1), end = c(0, NA, NA, 2))
    expected <- list(start = c(1, 0, NA, NA), end = c(2, NA, 0, NA))
    expect_equal(actual, expected)
})

test_that("[0,1] < [5,6] < [2,1] < [3,0] < [3,1] < [0,NA]", {
    actual <- f(start = c(2, 0, 3, 5, 0, 3), end = c(1, NA, 1, 6, 1, 0))
    expected <- list(start = c(0, 5, 2, 3, 3, 0), end = c(1, 6, 1, 0, 1, NA))
    expect_equal(actual, expected)
})
