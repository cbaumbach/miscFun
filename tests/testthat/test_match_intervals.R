context("match_intervals")

f <- match_intervals

test_that("happy path", {
    expect_equal(f(c(1, 2, 999), c(0, 1.5), c(3, 3), c("A", "B")), c("A", "A,B", ""))
})

test_that("NA positions result in NAs", {
    expect_equal(f(NA, 1, 2, "A"), NA_character_)
})

test_that("NAs in 'start' are ignored", {
    expect_equal(f(1, c(0, NA), c(2, 3), c("A", "B")), "A")
})

test_that("NAs in 'end' are ignored", {
    expect_equal(f(1, c(0, 1), c(1, NA), c("A", "B")), "A")
})

test_that("NAs in 'id' are ignored", {
    expect_equal(f(1, 0, 2, NA), "")
})

test_that("positions that don't fall into any interval result in an empty string", {
    expect_equal(f(999, 0, 1, "A"), "")
})
