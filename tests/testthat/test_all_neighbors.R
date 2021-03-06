context("all_neighbors")

test_that("it works for different kind of predicates", {
    expect_that(all_neighbors(identical, c(1,1,1)),    is_true())
    expect_that(all_neighbors(identical, c(1,2,1)),    is_false())
    expect_that(all_neighbors(identical, c(NA,NA,NA)), is_true())
    expect_that(all_neighbors(identical, c(1,NA,1)),   is_false())
    expect_that(all_neighbors(`<`, 1:10, 100:200), is_true())
    expect_that(all_neighbors(`<=`, list(a = 1, b = 3), list(c = 3, d = 5)), is_true())
    expect_that(all_neighbors(function(x,y) TRUE, 1:5),         is_true())
    expect_that(all_neighbors(function(x,y) FALSE, c(1,1,1)),   is_false())
    expect_that(all_neighbors(function(x,y) y - x == 1L, 1:10), is_true())
})
