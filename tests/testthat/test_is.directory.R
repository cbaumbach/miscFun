context("Testing the is.directory function")

test_that("is.directory works", {
    expect_that(is.directory("."), is_true())
    expect_that(is.directory(tempfile()), equals(NA))
    expect_that(is.directory(file.path(R.home("doc"), "COPYING")), is_false())
})
