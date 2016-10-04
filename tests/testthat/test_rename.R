context("rename")

test_that("happy path", {
    expect_equal(rename(c(A = 1, B = 2, C = 3), c(A = "a", B = "b")), c("a", "b", "C"))
})

test_that("the order of names in mapping can be different from the order of names of x", {
    expect_equal(rename(c(A = 1, B = 2), c(B = "b", A = "a")), c("a", "b"))
})

test_that("mapping may contain names not matching any names in x", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "a", C = "c")), c("a", "B"))
})

test_that("it is possible to swap the names of elements of x", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "B", B = "A")), c("B", "A"))
})

test_that("mapping may have a mixture of named and unnamed elements", {
    expect_equal(rename(c(A = 1), c("X", A = "a")), "a")
})

test_that("it is possible to give multiple elements of x the same name", {
    expect_equal(rename(c(A = 1, B = 2), c(A = "x", B = "x")), c("x", "x"))
})

test_that("x must have names", {
    expect_error(rename(1, c(A = "a")), "x must have names")
})

test_that("mapping must have names", {
    expect_error(rename(c(A = 1), "a"), "mapping must have names")
})

test_that("an error is thrown if mapping has duplicate names", {
    expect_error(rename(c(A = 1), c(B = "b", B = "c")), "mapping must not have duplicate names")
})

test_that("unnamed elements of x are never renamed", {
    expect_equal(rename(c(A = 1, 2), c(A = "a", "b")), c("a", ""))
})
