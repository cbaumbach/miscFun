context("ave2")

test_that("happy path", {
    xs <- c(1, 2, 3, 4, 5, 6, 7, 8)
    f1 <- c(1, 1, 1, 1, 0, 0, 0, 0)
    f2 <- c(1, 0, 1, 0, 1, 0, 1, 0)
    f <- function(x, sep) {
        paste(x, rev(x), sep = sep)
    }
    actual <- ave2(xs, list(f1, f2), f, "-")
    expected <- c("1-3", "2-4", "3-1", "4-2", "5-7", "6-8", "7-5", "8-6")
    expect_equal(actual, expected)
})

test_that("the \"factors\" argument must not be NULL", {
    expect_error(ave2(1, NULL, sum), "Argument \"factors\" must not be NULL.")
})

test_that("the \"factors\" argument must not be an empty list", {
    expect_error(ave2(1, list(), sum), "Argument \"factors\" must not be of length 0.")
})
