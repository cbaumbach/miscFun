context("find_matching_intervals")

f <- find_matching_intervals

expect_no_matching_interval <- function(match) {
    expect_equal(match$position, integer())
    expect_equal(match$interval, integer())
}

test_that("intervals can be nested", {
    actual <- f(c(-5, -2, 0, 2, 5), c(-3, -1), c(3, 1))
    expect_equal(actual$position, c(2, 3, 3, 4))
    expect_equal(actual$interval, c(1, 1, 2, 1))
})

test_that("intervals are closed", {
    actual <- f(0, 0, 0)
    expect_equal(actual$position, 1)
    expect_equal(actual$interval, 1)
})

test_that("NA's belong to no interval", {
    expect_no_matching_interval(f(NA, 0, 0))
})

test_that("nothing belongs to an interval with missing lower or upper limit", {
    expect_no_matching_interval(f(0, NA, NA))
    expect_no_matching_interval(f(0, NA, 0))
    expect_no_matching_interval(f(0, 0, NA))
})

test_that("nothing falls into an empty interval", {
    expect_no_matching_interval(f(0, 1, -1))
})

test_that("start positions don't have to be sorted", {
    actual <- f(0, c(99, 0), c(99, 0))
    expect_equal(actual$interval, 2)
})

test_that("we are not limited to the range of integers", {
    x <- .Machine$integer.max + 2
    expect_equal(f(x, x, x)$interval, 1)
})
