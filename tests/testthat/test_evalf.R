context("evalf")

test_that("evalf", {
    x <- 1; y <- 2
    expect_equal(evalf("%s + y", "x"), 3)
})
