

context("Matching positions to intervals")

test_that("everything works fine in the vanilla use case", {
    pos   <- c(1L,2L,3L)
    start <- c(0L,1L,4L)
    end   <- c(1L,2L,5L)
    id    <- c(1L,2L,3L)

    expect_that(match_intervals(pos, start, end, id, quiet = TRUE), equals(c("1,2", "2", "")))
})

test_that("we get NAs of type 'character' for NA positions", {
    pos   <- c(1L,NA,3L)
    start <- c(0L,1L,4L)
    end   <- c(1L,2L,5L)
    id    <- c(1L,2L,3L)

    x <- match_intervals(pos, start, end, id, quiet = TRUE)

    expect_that(is.na(x[2]) && typeof(x[2]) == "character", is_true())
})

test_that("we ignore missing values in start, end, or id", {
    pos   <- c(1L,2L,3L)
    start <- c(0L,NA,4L,0L)             # missing start position
    end   <- c(1L,2L,5L,2L)
    id    <- c(1L,2L,3L,4L)

    x1 <- match_intervals(pos, start, end, id, quiet = TRUE)

    pos   <- c(1L,2L,3L)
    start <- c(0L,1L,4L,0L)
    end   <- c(1L,NA,5L,2L)             # missing end position
    id    <- c(1L,2L,3L,4L)

    x2 <- match_intervals(pos, start, end, id, quiet = TRUE)

    pos   <- c(1L,2L,3L)
    start <- c(0L,1L,4L,0L)
    end   <- c(1L,2L,5L,2L)
    id    <- c(1L,NA,3L,4L)             # missing id

    x3 <- match_intervals(pos, start, end, id, quiet = TRUE)

    pos   <- c(1L,2L,3L)
    start <- c(NA,NA,NA,NA)                # no valid interval
    end   <- c(1L,2L,5L,2L)
    id    <- c(1L,2L,3L,4L)

    x4 <- match_intervals(pos, start, end, id, quiet = TRUE)

    expect_that(x1, equals(c("1,4", "4", "")))
    expect_that(x2, equals(c("1,4", "4", "")))
    expect_that(x3, equals(c("1,4", "4", "")))
    expect_that(x4, equals(c("","","")))
})
