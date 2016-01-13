context("make pairs")

test_that("ordered pairs from single argument", {
    x <- 1:3
    expect_equal(make_ordered_pairs(x)$x, c(1, 2, 3, 1, 2, 3, 1, 2, 3))
    expect_equal(make_ordered_pairs(x)$y, c(1, 1, 1, 2, 2, 2, 3, 3, 3))
})

test_that("unordered pairs from single argument", {
    x <- 1:3
    expect_equal(make_unordered_pairs(x)$x, c(1, 2, 3, 2, 3, 3))
    expect_equal(make_unordered_pairs(x)$y, c(1, 1, 1, 2, 2, 3))
})

test_that("ordered pairs from two arguments of unequal length", {
    x <- 1:3; y <- 1:2
    expect_equal(make_ordered_pairs(x, y)$x, c(1, 2, 3, 1, 2, 3))
    expect_equal(make_ordered_pairs(x, y)$y, c(1, 1, 1, 2, 2, 2))
})

test_that("unordered pairs from two arguments of unequal length", {
    x <- 1:3; y <- 1:2
    expect_equal(make_unordered_pairs(x, y)$x, c(1, 2, 3, 2, 3))
    expect_equal(make_unordered_pairs(x, y)$y, c(1, 1, 1, 2, 2))
})
