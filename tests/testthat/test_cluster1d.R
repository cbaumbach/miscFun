context("Testing the cluster1d function")

test_that("cluster1d works", {
    x <- c(9, 12, 3, 4, 17, 10, 12, NA, 6, 1)
    xcl <- c(2L, 2L, 1L, 1L, 3L, 2L, 2L, NA, 1L, 1L)
    gap <- 3
    frac <- 3 / diff(range(x, na.rm = TRUE))

    expect_that(cluster1d(x, gap = gap), is_identical_to(xcl))
    expect_that(cluster1d(x, frac = frac), is_identical_to(xcl))
    expect_that(cluster1d(sort(x), gap = gap), is_identical_to(sort(xcl)))
    expect_that(cluster1d(sort(x), frac = frac), is_identical_to(sort(xcl)))
})
