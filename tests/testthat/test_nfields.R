context("nfields")

test_that("nfields works with different separators", {
    contents <- paste(c(
        "one two three four",
        "1 2 3 4",
        "a b c d"), sep = "\n")

    expect_that(nfields(textConnection(gsub(" ", "\t", contents)), sep = "\t"), equals(4L))
    expect_that(nfields(textConnection(gsub(" ", ",",  contents)), sep = ","),  equals(4L))
    expect_that(nfields(textConnection(contents),                  sep = " "),  equals(4L))
})
