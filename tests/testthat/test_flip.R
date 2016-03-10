context("flip")

test_that("only valid functions are flipped", {
    f <- function(x, y) x + y

    expect_that((flip(function(x, y) paste0(x, y)))(1,2), equals("21"))
    expect_that((flip(function(x, y, ...) paste0(x, y, ...)))(1,2,3), equals("213"))
    expect_that((flip(function(x, y, ..., z) paste0(x, y, z, ...)))(1, 2, 3, z = 4), equals("2143"))
    expect_that(identical(environment(flip(f)), environment(f)), is_true())
    expect_that(identical(body(flip(f)), body(f)), is_true())
    expect_that(flip(function()  1L), throws_error("Function must take at least 2 arguments."))
    expect_that(flip(function(x) 1L), throws_error("Function must take at least 2 arguments."))
    expect_that(flip(function(x, ...)    1L), throws_error("Function must not have \"...\" as 1st or 2nd argument."))
    expect_that(flip(function(..., x)    1L), throws_error("Function must not have \"...\" as 1st or 2nd argument."))
    expect_that(flip(function(x, ..., y) 1L), throws_error("Function must not have \"...\" as 1st or 2nd argument."))
    expect_that(flip(function(..., x, y) 1L), throws_error("Function must not have \"...\" as 1st or 2nd argument."))
})
