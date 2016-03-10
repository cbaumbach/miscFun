context("ave2")

test_that("ave2 works", {

    x  <- 1:8
    f1 <- rep(1:2, each = 4L)
    f2 <- rep_len(c("A","B"), 8L)

    expect_that(ave2(x, list(f1), sum),     equals(c(10L, 10L, 10L, 10L, 26L, 26L, 26L, 26L)))
    expect_that(ave2(x, list(f1), sum, 1L), equals(c(11L, 11L, 11L, 11L, 27L, 27L, 27L, 27L)))
    expect_that(ave2(x, list(f2), sum),     equals(c(16L, 20L, 16L, 20L, 16L, 20L, 16L, 20L)))
    expect_that(ave2(x, list(f2), sum, 1L), equals(c(17L, 21L, 17L, 21L, 17L, 21L, 17L, 21L)))
    expect_that(ave2(x, list(f1, f2), sum),         equals(c(4L, 6L, 4L, 6L, 12L, 14L, 12L, 14L)))
    expect_that(ave2(x, list(f1, f2), sum, 1L),     equals(c(5L, 7L, 5L, 7L, 13L, 15L, 13L, 15L)))
    expect_that(ave2(x, list(f1, f2), sum, 1L, 2L), equals(c(7L, 9L, 7L, 9L, 15L, 17L, 15L, 17L)))
    expect_that(ave2(x, list(), sum), throws_error("`factors' must not be of length 0."))
    expect_that(ave2(x, f = sum),     throws_error("`factors' must not be missing."))
    expect_that(ave2(x, NULL, sum),   throws_error("`factors' must not be `NULL'."))
})
