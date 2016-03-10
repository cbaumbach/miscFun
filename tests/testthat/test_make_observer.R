context("make_observer")

test_that("make_observer works", {
    seen <- make_observer()

    expect_that(seen(1), is_false())
    expect_that(seen(2), is_false())
    expect_that(seen(0), is_false())
    expect_that(seen(3), is_false())
    expect_that(seen(1), is_true())
    expect_that(seen(1), is_true())
    expect_that(seen(3), is_true())
    expect_that(seen(4), is_false())
    expect_that(seen(show = TRUE), equals(as.character(0:4)))
    expect_that(seen(NA), throws_error('Argument must be of length 1 and class "character", "numeric", or "integer".'))
})
