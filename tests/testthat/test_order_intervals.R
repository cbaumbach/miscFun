context("order_intervals")

random_intervals <- function(seed = 12345L) {
    n <- 100L
    size <- 20L
    number_of_nas <- 5L
    draw <- function() {
        sample.int(n, size, replace = FALSE)
    }
    draw_na <- function() {
        sample.int(size, number_of_nas, replace = TRUE)
    }
    set.seed(seed)
    start <- draw()
    end <- draw()
    start[draw_na()] <- NA
    end[draw_na()] <- NA
    list(start = start, end = end)
}

test_that("ordered intervals are sorted", {
    x <- random_intervals()
    start <- x$start
    end <- x$end
    ord <- order_intervals(start, end)
    sorted <- sort_intervals(start, end)

    expect_equal(start[ord], sorted$start)
    expect_equal(end[ord], sorted$end)
})

test_that("sorting can be reversed", {
    x <- random_intervals()
    start <- x$start
    end <- x$end
    ord <- order_intervals(start, end)
    old_order <- order(ord)
    sorted <- sort_intervals(start, end)

    expect_equal(sorted$start[old_order], start)
    expect_equal(sorted$end[old_order], end)
})
