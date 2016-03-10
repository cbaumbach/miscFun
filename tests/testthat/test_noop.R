context("noop")

test_that("noop works", {
    expect_that(noop(stop()), equals(NULL))
    expect_that(noop(warning()), equals(NULL))
    expect_that(noop(exit()), equals(NULL))
    expect_that(noop(undefined_value), equals(NULL))
    expect_that(noop(undefined_function()), equals(NULL))
    expect_that(noop(1, 2, 3), equals(NULL))
    expect_that(noop(1, 2, 3, stop(), warning(), exit()), equals(NULL))
})
