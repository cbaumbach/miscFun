context("Output the first lines of gzip-files")

test_that("gzhead works", {
    tmp1 <- tempfile(tmpdir = ".")
    tmp2 <- tempfile(tmpdir = ".")
    m1 <- paste("line", 1:3)
    m2 <- paste(toupper("line"), 1:4)
    cat(m1, file = tmp1, sep = "\n")
    cat(m2, file = tmp2, sep = "\n")

    tryCatch({
        expect_that(gzhead(c(tmp1, tmp2)),                          equals(c(m1[1L],     m2[1L])))
        expect_that(gzhead(c(tmp1, tmp2), simplify = FALSE),        equals(list(m1[1L],  m2[1L])))
        expect_that(gzhead(c(tmp1, tmp2), n = 2L),                  equals(list(m1[1:2], m2[1:2])))
        expect_that(gzhead(c(tmp1, tmp2), n = 2L, simplify = TRUE), equals(list(m1[1:2], m2[1:2])))
        expect_that(gzhead(c("foo", "bar")), throws_error("Some `files' don't exist:"))
    }, finally = file.remove(tmp1, tmp2))
})
