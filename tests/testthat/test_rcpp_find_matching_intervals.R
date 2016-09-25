context("rcpp_find_matching_intervals")

empty <- list(integer(), integer())

f <- rcpp_find_matching_intervals

test_that("the components of the returned list are called 'position' and 'interval'", {
    actual <- names(f(0L, 0L, 0L))
    expected <- c("position", "interval")
    expect_equal(actual, expected)
})

test_that("nested intervals are handled correctly", {
    start <- c(-3L, -1L)
    end   <- c(+3L, +1L)
    positions <- c(-5L, -2L, 0L, 2L, 5L)
    actual <- f(positions, start, end)
    expected <- list(c(2L, 3L, 3L, 4L),
                     c(1L, 1L, 2L, 1L))
    expect_equivalent(actual, expected)
})

test_that("intervals are closed", {
    expect_equivalent(f(0L, 0L, 0L), list(1L, 1L))
})

test_that("NA's belong to no interval", {
    expect_equivalent(f(NA, 0L, 0L), empty)
})

test_that("nothing belongs to [NA, NA]", {
    expect_equivalent(f(0L, NA, NA), empty)
})

test_that("nothing belongs to [NA, *]", {
    expect_equivalent(f(0L, NA, 0L), empty)
})

test_that("nothing belongs to [*, NA]", {
    expect_equivalent(f(0L, 0L, NA), empty)
})

test_that("nothing falls into an empty interval", {
    expect_equivalent(f(0L, 1L, -1L), empty)
})

test_that("start positions don't have to be sorted", {
    actual <- f(0L, c(99L, 0L), c(99L, 0L))
    expect_equal(actual$interval, 2)
})
