context("find_duplicates")

f <- find_duplicates

read_table <- function(text) {
    utils::read.table(text = text, header = TRUE)
}

test_that("every row is a duplicate", {
    data <- read_table("
    x y
    1 2
    1 2")
    expect_identical(f(data, "x"), data)
    expect_identical(f(data, "y"), f(data, "x"))
})

test_that("even when selecting a single column we still get back a data frame", {
    data <- read_table("
    x y
    1 2
    1 2")
    expect_identical(f(data, "x", "y"), data[, "y", drop = FALSE])
})

test_that("row names are conserved", {
    data <- read_table("
    x y
    1 2
    - -
    1 2")
    expect_identical(rownames(f(data, "x")), c("1", "3"))
    expect_identical(rownames(f(data, c("x", "y"))), c("1", "3"))
})

test_that("pairs of duplicate rows", {
    data <- read_table("
    x y z
    1 2 3
    1 2 -
    1 - 3")
    expect_identical(f(data, c("x", "y")), data[c(1, 2), ])
    expect_identical(f(data, c("x", "z")), data[c(1, 3), ])
})

test_that("repeated NA values are considered as duplicates", {
    data <- read_table("
    x  y
    NA 2
    NA 2")
    expect_identical(f(data, "x"), data)
})

test_that("NA values never count as duplicates of non-NA values", {
    data <- read_table("
    x  y
    1  2
    NA 2")
    expect_equal(nrow(f(data, "x")), 0)
})

test_that("factors are handled correctly", {
    data <- read_table("
    x  y z
    NA 2 a
    1  3 b
    1  2 b")
    expect_identical(f(data, "x"), data[c(2, 3), ])
    expect_identical(f(data, "x", c("y", "z")), data[c(2, 3), c("y", "z")])
    expect_identical(f(data, c("x", "y")), data[0, ])
    expect_identical(f(data, c("x", "z")), data[c(2, 3), ])
})

test_that("character vectors are handled correctly", {
    data <- read_table("
    x  y z
    NA 2 a
    1  3 b
    1  2 b")
    expect_identical(f(data, "x"), data[c(2, 3), ])
    expect_identical(f(data, "x", c("y", "z")), data[c(2, 3), c("y", "z")])
    expect_identical(f(data, c("x", "y")), data[0, ])
    expect_identical(f(data, c("x", "z")), data[c(2, 3), ])
})

test_that("we can deal with empty dataframes", {
    data <- read_table("
    x y z")
    expect_identical(f(data, "x"), data[0, ])
    expect_identical(f(data, "x", c("y", "z")), data[0, c("y","z")])
    expect_identical(f(data, c("x", "y")), data[0, ])
    expect_identical(f(data, c("x", "z")), data[0, ])
})

test_that("we throw an error when finding non-existing columns", {
    data <- read_table("
    x y z
    1 2 3
    4 5 6
    7 8 9")
    expect_error(f(data, "w"), "nonexistent variables in COLUMNS")
    expect_error(f(data, "w", c("y", "z")), "nonexistent variables in COLUMNS")
    expect_error(f(data, "x", c("w", "z")), "nonexistent variables in SELECT")
})
