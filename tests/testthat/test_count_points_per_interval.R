context("count_points_per_interval")

f <- count_points_per_interval

test_that("[NA, NA] contains no points", {
    expect_equal(f(0, NA, NA), 0)
})

test_that("[0, 0] contains 0", {
    expect_equal(f(0, 0, 0), 1)
})

test_that("[NA, *] contains no points", {
    expect_equal(f(0, NA, 0), 0)
})

test_that("[*, NA] contains no points", {
    expect_equal(f(0, 0, NA), 0)
})

test_that("an error is thrown if start and end are not of the same length", {
    expect_error(f(0, 1, 2:3), "START and END must have same length")
})

test_that("unsorted intervals are allowed", {
    actual <- f(0:9, c(2, 0, 1), c(5, 4, 1))
    expected <- c(4, 5, 1)
    expect_equal(actual, expected)
})

test_that("unsorted points are allowed", {
    actual <- f(c(2, -1, 0, 3, 1), c(0, 0, 0), c(1, 2, 3))
    expected <- c(2, 3, 4)
    expect_equal(actual, expected)
})

test_that("the number of points in Inf intervals is NA", {
    expect_equal(f(0, 0, Inf), NA_integer_)
    expect_equal(f(0, -Inf, 0), NA_integer_)
    expect_equal(f(0, -Inf, Inf), NA_integer_)
})

test_that("Inf points are ignored", {
    expect_equal(f(Inf, 0, 0), 0)
    expect_equal(f(-Inf, 0, 0), 0)
})

test_that("duplicate points are included in the count", {
    expect_equal(f(c(0, 0), 0, 0), 2)
})

test_that("duplicate intervals are allowed", {
    expect_equal(f(0, c(0, 0), c(0, 0)), c(1, 1))
})
