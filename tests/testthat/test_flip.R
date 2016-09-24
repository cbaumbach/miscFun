context("flip")

f <- function(x, y, ...) {
    paste(x, y, ...)
}

test_that("happy path", {
    expect_identical(flip(f)(1, 2, 3), "2 1 3")
})

test_that("flip(f) and f have the same environment", {
    expect_identical(environment(flip(f)), environment(f))
})

test_that("flip(f) and f have the same body", {
    expect_identical(body(flip(f)), body(f))
})

test_that("the function to be flipped must take at least 2 arguments", {
    error_message <- "Function must take at least 2 arguments."
    expect_error(flip(function() 1), error_message)
    expect_error(flip(function(x) 1), error_message)
})

test_that("the function to be flipped doesn't have \"...\" as 1st or 2nd argument", {
    error_message <- "Function must not have \"...\" as 1st or 2nd argument."
    expect_error(flip(function(...) 1), error_message)
    expect_error(flip(function(x, ...) 1), error_message)
    expect_error(flip(function(..., x) 1), error_message)
})
