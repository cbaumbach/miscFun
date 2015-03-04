context("Inverting maps.")

test_that("simple cases work as expected", {
    map <- list(a = c("A","C"), b = c("B","C"))

    expect_that(rev_map(map), equals(
        list(A = "a", B = "b", C = c("a","b"))))
})

test_that("\"NA\" tags are turned into \"NA\" strings", {
    map <- list(a = c("A","C"), "NA" = c("A","C"), b = c("B","C"))

    ## NA's come last because of sorting.
    expect_that(rev_map(map), equals(
        list(A = c("a","NA"), B = "b", C = c("a","b","NA"))))
})

test_that("missing values are dropped", {
    map <- list(a = c(NA,"A","C"), b = c("B","C",NA))

    expect_that(rev_map(map), equals(
        list(A = "a", B = "b", C = c("a","b"))))
})
