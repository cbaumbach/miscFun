context("group_words")

test_that("group_words works", {
    s <- "The quick yellow-orange-brown fox jumps over the 3-year-old, lazy dog."

    ## Split on white space or after hyphens.
    x <- group_words(s, 10, split = "\\s+|(?<=-)", special = "-$")

    expect_that(length(x), equals(1L))
    expect_that(all(vapply(x[[1L]], nchar, integer(1L)) <= 10L), is_true())
    expect_that(!any(grepl("^\\s+", x[[1L]], perl = TRUE)), is_true())
    expect_that(!any(grepl("\\s+$", x[[1L]], perl = TRUE)), is_true())
    expect_that(!any(grepl("^[[:punct:]]", x[[1L]])), is_true())
})
