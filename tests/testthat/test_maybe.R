context("maybe")

test_that("maybe works", {

    expect_that(maybe(as.integer, "12"),   equals(12L))
    expect_that(maybe(as.double,  "1e-2"), equals(1e-2))
    expect_that(maybe(as.integer, "foo"),  equals("foo"))
    expect_that(maybe(as.double,  "foo"),  equals("foo"))
    expect_that(maybe(warning,    "foo"),  equals("foo"))
    expect_that(maybe(stop,       "foo"),  equals("foo"))
    expect_that(maybe(as.integer, stop("x is rotten")), throws_error("x is rotten"))
    expect_that(maybe(as.integer, warning("x is rotten")), gives_warning("x is rotten"))

    expect_that(maybe(as.integer, "12", 0L),  equals(12L))
    expect_that(maybe(as.double,  "1e-2", 0), equals(1e-2))
    expect_that(maybe(as.integer, "foo", 0L), equals(0L))
    expect_that(maybe(as.double,  "foo", 0),  equals(0))
    expect_that(maybe(warning,    "foo", 0L), equals(0))
    expect_that(maybe(stop,       "foo", 0L), equals(0))
    expect_that(maybe(as.integer, stop("x is rotten"), 0L), throws_error("x is rotten"))
    expect_that(maybe(as.integer, warning("x is rotten"), 0L), gives_warning("x is rotten"))
})
