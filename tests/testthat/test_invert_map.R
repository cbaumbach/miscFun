context("invert_map")

test_that("happy path", {
    x <- invert_map(list(a = c("A", "C"), b = c("B", "C")))
    expect_equal(x$A, "a")
    expect_equal(x$B, "b")
    expect_equal(x$C, c("a", "b"))
})

test_that("NA tags are turned into NA strings", {
    x <- invert_map(list("NA" = "A"))
    expect_equal(x$A, "NA")
})

test_that("NA values are dropped", {
    x <- invert_map(list(a = c(NA, "A")))
    expect_equal(names(x), "A")
})

test_that("we get an empty map if all values are NA", {
    x <- invert_map(list(a = NA))
    expect_true(is.list(x))
    expect_equal(length(x), 0)
})

test_that("if there are no NA values the inverse of the inverse is the map itself modulo the order of elements", {
    map <- list(a = c("A", "B"), b = c("B", "C"))
    expect_equal(invert_map(invert_map(map)), map)
})

test_that("inverting a map twice removes keys without values", {
    x <- invert_map(invert_map(list(a = "A", b = NULL, c = integer())))
    expect_equal(names(x), "a")
})
