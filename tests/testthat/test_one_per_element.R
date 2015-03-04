context("One value per vector element")

test_that("the simplest case works", {

    x <- c("1,2", "3,4,5", "6")
    y <- c("1---2", "3--4----5", "6-7")

    expect_that(one_per_element(x), equals(as.character(1:6)))
    expect_that(one_per_element(y, sep = "-+", perl = TRUE), equals(as.character(1:7)))
})

test_that("NAs are preserved", {

    x <- c("1,2", "3,4,5", NA, "6")

    expect_that(one_per_element(x), equals(as.character(c(1:5,NA,6L))))
})
