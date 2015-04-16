context("Maybe apply a function to a value")

test_that("maybe works", {

    expect_that(maybe(as.integer, "12"),   equals(12L))
    expect_that(maybe(as.double,  "1e-2"), equals(1e-2))
    expect_that(maybe(as.integer, "foo"),  equals("foo"))
    expect_that(maybe(as.double,  "foo"),  equals("foo"))
    expect_that(maybe(warning,    "foo"),  equals("foo"))
})
