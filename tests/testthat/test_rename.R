context("rename")

test_that("rename works", {
    d <- expand.grid(x = 1:2, y = c(TRUE, FALSE))

    expect_that(rename(d), throws_error("argument \"old2new\" is missing, with no default"))
    expect_that(rename(d, c("a", "b", "c")), gives_warning("`names' attribute of `old2new' is NULL."))
    expect_that(rename(d, c("a", "b", "c"), warn = FALSE), equals(names(d)))
    expect_that(rename(d, c(x = "a", y = "b")), equals(c("a", "b")))
    expect_that(rename(d, c(y = "b", x = "a")), equals(c("a", "b")))
    expect_that(rename(d, c(foo = "oof", y = "z", bar = "rab")), gives_warning("The following name tags from `old2new' don't match any element of `names\\(x\\)': \"foo\", \"bar\""))
    expect_that(rename(d, c(foo = "oof", y = "z", bar = "rab"), warn = FALSE), equals(c("x", "z")))
    expect_that(rename(d, c(x = "y", y = "x")), equals(c("y", "x")))
    expect_that(rename(d, c(x = "a", "b")), equals(c("a", "y")))
    expect_that(rename(d, c("b", x = "a")), equals(c("a", "y")))
    expect_that(rename(cbind(d, d), c(x = "a", y = "b")), gives_warning("Duplicates after renaming: \"a\", \"b\""))
    expect_that(rename(cbind(d, d), c(x = "a", y = "b"), warn = FALSE), equals(rep_len(c("a", "b"), 4L)))
    expect_that(rename(d, c(x = "y")), gives_warning("Duplicates after renaming: \"y\""))
})
