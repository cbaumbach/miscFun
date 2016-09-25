context("rcpp_find_matching_intervals")

empty <- list(integer(), integer())

f <- rcpp_find_matching_intervals

test_that("the components of the returned list are called 'position' and 'interval'", {
    actual <- names(f(0, 0, 0))
    expected <- c("position", "interval")
    expect_equal(actual, expected)
})

test_that("nested intervals are handled correctly", {
    start <- c(-3, -1)
    end   <- c(+3, +1)
    positions <- c(-5, -2, 0, 2, 5)
    actual <- f(positions, start, end)
    expected <- list(c(2, 3, 3, 4),
                     c(1, 1, 2, 1))
    expect_equivalent(actual, expected)
})

test_that("intervals are closed", {
    expect_equivalent(f(0, 0, 0), list(1, 1))
})

test_that("NA's belong to no interval", {
    expect_equivalent(f(NA, 0, 0), empty)
})

test_that("nothing belongs to [NA, NA]", {
    expect_equivalent(f(0, NA, NA), empty)
})

test_that("nothing belongs to [NA, *]", {
    expect_equivalent(f(0, NA, 0), empty)
})

test_that("nothing belongs to [*, NA]", {
    expect_equivalent(f(0, 0, NA), empty)
})

test_that("nothing falls into an empty interval", {
    expect_equivalent(f(0, 1, -1), empty)
})

test_that("start positions don't have to be sorted", {
    actual <- f(0, c(99, 0), c(99, 0))
    expect_equal(actual$interval, 2)
})

test_that("we are not limited to the range of integers", {
    x <- .Machine$integer.max + 2
    expect_equal(f(x, x, x)$interval, 1)
})
