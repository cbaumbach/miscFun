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

test_that("the reverse of the reverse of a map without missing values is the map itself", {
    map <- list(a = c("A","C"), b = c("B","C"))
    rrmap <- rev_map(rev_map(map))

    ## Sort elements.
    map   <- lapply(map, sort)
    rrmap <- lapply(rrmap, sort)

    expect_that(rrmap, equals(map))
})

test_that("reversing a map twice removes keys without values", {
    map <- list(a = c("A","C"), b = c("B","C"), c = NULL, d = character(0L))
    map2 <- list(a = c("A","C"), b = c("B","C"))
    rrmap <- rev_map(rev_map(map))

    sort_elements <- function(x) {
        if (is.null(x))
            x
        else
            sort(x)
    }
    map   <- lapply(map, sort_elements)
    map2  <- lapply(map2, sort_elements)
    rrmap <- lapply(rrmap, sort_elements)

    expect_that(rrmap, equals(map2))
})
