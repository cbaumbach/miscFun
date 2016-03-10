context("submatch")

test_that("submatch works", {
    xs <- c("chr1_path/to/the_chr10_3_foo.txt",
            "chr2_path/to/the_chr7_5_bar.txt",
            "chrA_path/to/the_chrX_Y_baz.txt")

    pattern1 <- ".*chr(\\d+)_(\\d+)"
    pattern2 <- ".*chr(\\d+)"

    m1 <- rbind(c("10", "3"), c("7", "5"), c(NA, NA))
    m2 <- matrix(c("10", "7", NA), ncol = 1L)
    m3 <- matrix(c("10", "3"), nrow = 1L)
    m4 <- matrix(as.character(c(NA, NA)), nrow = 1L)

    expect_that(submatch("a_\\(b\\)_[c(d)]_f", "any"), throws_error("`pattern' must contain a parenthesized subexpression."))
    expect_that(submatch(pattern1, xs), equals(m1))
    expect_that(submatch(pattern2, xs), equals(m2))
    expect_that(submatch(pattern2, xs, drop = TRUE), equals(as.vector(m2)))
    expect_that(submatch(pattern1, xs[1L]), equals(m3))
    expect_that(submatch(pattern1, xs[1L], drop = TRUE), equals(as.vector(m3)))
    expect_that(submatch(pattern1, xs[3L]), equals(m4))
    expect_that(submatch(pattern1, xs[3L], drop = TRUE), equals(as.vector(m4)))
})
