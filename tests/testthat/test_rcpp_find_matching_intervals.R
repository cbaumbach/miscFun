context("rcpp_find_matching_intervals")

empty <- list(integer(), integer())

test_that("the components of the returned list are called 'position' and 'interval'", {
    actual <- names(rcpp_find_matching_intervals(0, 0, 0))
    expected <- c("position", "interval")
    expect_equal(actual, expected)
})

test_that("nested intervals are handled correctly", {
    start <- c(-3L, -1L)
    end   <- c(+3L, +1L)
    positions <- c(-5L, -2L, 0L, 2L, 5L)
    actual <- rcpp_find_matching_intervals(positions, start, end)
    expected <- list(c(2L, 3L, 3L, 4L),
                     c(1L, 1L, 2L, 1L))
    expect_equivalent(actual, expected)
})

test_that("intervals are closed", {
    expect_equivalent(rcpp_find_matching_intervals(0L, 0L, 0L), list(1L, 1L))
})

test_that("NA's belong to no interval", {
    expect_equivalent(rcpp_find_matching_intervals(NA, 0L, 0L), empty)
})

test_that("nothing belongs to [NA, NA]", {
    expect_equivalent(rcpp_find_matching_intervals(0L, NA, NA), empty)
})

test_that("nothing belongs to [NA, *]", {
    expect_equivalent(rcpp_find_matching_intervals(0L, NA, 0L), empty)
})

test_that("nothing belongs to [*, NA]", {
    expect_equivalent(rcpp_find_matching_intervals(0L, 0L, NA), empty)
})

test_that("nothing falls into an empty interval", {
    expect_equivalent(rcpp_find_matching_intervals(0L, 1L, -1L), empty)
})

test_that("the code runs fast", {
    nintervals <- 1000L
    start <- 2L * seq(0L, nintervals - 1L)
    end <- start + 1L
    pos <- start

    x <- microbenchmark::microbenchmark(times = 20,
        actual <- rcpp_find_matching_intervals(pos, start, end))

    max_time <- 1e7  # nano seconds
    median_time <- median(x$time)
    expected <- list(position = seq_len(nintervals),
        interval = seq_len(nintervals))

    expect_less_than(median_time, max_time, label = "TOO SLOW:")
    expect_equal(actual, expected, label = "WRONG RESULT:")
})
