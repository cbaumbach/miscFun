context("rename")

test_that("happy path", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "a", B = "b")), c("a", "b"))
})

test_that("we get a warning if old2new has no names attribute", {
    expect_warning(rename(1, 2), "`names' attribute of `old2new' is NULL.")
})

test_that("if old2new has no `names' attribute, `rename' is a no-op", {
    expect_equal(rename(c(A = 1, B = 2), 1, warn = FALSE), c("A", "B"))
})

test_that("the order of names in old2new can be different from the order of names of x", {
    expect_equal(rename(c(A = 1, B = 2), c(B = "b", A = "a")), c("a", "b"))
})

test_that("we get a warning for names of old2nem that don't match any names in x", {
    expect_warning(rename(c(A = 1), c(B = "b")), "The following name tags from `old2new' don't match any element of `names\\(x\\)': \"B\"")
})

test_that("old2new may contain names not matching any names in x", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "a", C = "c"), warn = FALSE), c("a", "B"))
})

test_that("it is possible to swap the names of elements of x", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "B", B = "A")), c("B", "A"))
})

test_that("old2new may have a mixture of named and unnamed elements", {
    expect_equal(rename(c(A = 1), c("X", A = "a")), "a")
})

test_that("we get a warning if renaming would result in duplicate names", {
    expect_warning(rename(c(A = 1, B = 2), c(A = "C", B = "C")), "Duplicates after renaming: \"C\"")
    expect_warning(rename(c(A = 1, B = 2), c(A = "B")), "Duplicates after renaming: \"B\"")
})

test_that("it is possible to give multiple elements of x the same name", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "x", B = "x"), warn = FALSE), c("x", "x"))
})
