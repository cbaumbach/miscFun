context("nsubexp")

test_that("nsubexp works", {
    expect_that(nsubexp(""),                                equals(0L))
    expect_that(nsubexp(" "),                               equals(0L))
    expect_that(nsubexp("[()]"),                            equals(0L))
    expect_that(nsubexp("([()])"),                          equals(1L))
    expect_that(nsubexp("(()[()])"),                        equals(2L))
    expect_that(nsubexp("an_(expr)_with_(4)_sub(ex(p)r)s"), equals(4L))
})
