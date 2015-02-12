context("Flattening csv strings")

test_that("flatten_csv works with fixed and regex separators", {
    x1 <- c("1,2,3", "4,5", "6")
    x2 <- c("1abc2de3", "4f5", "6")
    x3 <- c("123", "45", "6")

    expect_that(flatten_csv(x1),                                      equals(as.character(1:6)))
    expect_that(flatten_csv(x2, sep = "[[:alpha:]]+", fixed = FALSE), equals(as.character(1:6)))
    expect_that(flatten_csv(x3, sep = "",             fixed = FALSE), equals(as.character(1:6)))
})
