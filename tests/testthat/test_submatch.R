context("Extracting submatches from strings")

test_that("submatch works", {
    x <- submatch(".*chr(\\d+)_(\\d+).*", c("path/to/the_chr10_3_foo.txt",
                                            "path/to/the_chr7_5_bar.txt",
                                            "path/to/the_chrX_Y_baz.txt"))

    m <- rbind(c("10", "3"), c("7", "5"), c(NA, NA))

    expect_that(submatch("a_\\(b\\)_[c(d)]_f", "any"), throws_error("`pattern' must contain a parenthesized subexpression."))
    expect_that(x[],                   equals(m))
    expect_that(x[, ],                 equals(m))
    expect_that(x[, 1:2],              equals(m[, 1:2]))
    expect_that(x[, 2],                equals(m[, 2]))
    expect_that(x[, 2, drop = FALSE],  equals(m[, 2, drop = FALSE]))
    expect_that(x[1, ],                equals(m[1, ]))
    expect_that(x[1, , drop = FALSE],  equals(m[1, , drop = FALSE]))
    expect_that(x[3, 1],               equals(m[3, 1]))
    expect_that(x[3, 1, drop = FALSE], equals(m[3, 1, drop = FALSE]))
    expect_that(x[1, 1],               equals(m[1, 1]))
    expect_that(x[1, 1, drop = FALSE], equals(m[1, 1, drop = FALSE]))
    expect_that(x[1:2, 1:2],           equals(m[1:2, 1:2]))
})
