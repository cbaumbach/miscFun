context("is_duplicated_in")

test_that("happy path", {
    expect_equal(is_duplicated_in(c(1, 2, 1, NA)), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("we get all FALSE if all elements are unique", {
    expect_equal(is_duplicated_in(c(1, 2, 3)), c(FALSE, FALSE, FALSE))
})

test_that("we get all TRUE if every element occurs more than once", {
    expect_equal(is_duplicated_in(c(1, 2, 1, 2)), c(TRUE, TRUE, TRUE, TRUE))
})

test_that("NAs always result in FALSE", {
    expect_equal(is_duplicated_in(c(NA, NA)), c(FALSE, FALSE))
})

test_that("a length-0 input results in a length-0 logical vector", {
    expect_equal(is_duplicated_in(integer()), logical())
})

test_that("NULL results in a length-0 logical vector", {
    expect_equal(is_duplicated_in(NULL), logical())
})

test_that("the argument can also be a list", {
    expect_equal(is_duplicated_in(list(1, 2, 1, NA)), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("we can deal with nested lists", {
    expect_equal(is_duplicated_in(list(list(1, 2), 3, list(1, 2))), c(TRUE, FALSE, TRUE))
})
