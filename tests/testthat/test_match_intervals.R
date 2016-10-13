context("match_intervals")

f <- match_intervals

test_that("happy path", {
    expect_equal(f(
        point = c(1, 2, 999),
        start = c(0, 1.5),
        end = c(3, 3)
    ), list(1, c(1, 2), integer()))
})

test_that("NA positions result in NAs", {
    expect_equal(f(NA, 1, 2), list(NA))
})

test_that("NAs in 'start' are ignored", {
    expect_equal(f(1, c(0, NA), c(2, 3)), list(1))
})

test_that("NAs in 'end' are ignored", {
    expect_equal(f(1, c(0, 1), c(1, NA)), list(1))
})

test_that("positions that don't fall into any interval result in an empty vector", {
    expect_equal(f(999, 0, 1), list(integer()))
})
