context("quoting")

test_that("inner quotes are escaped as needed", {
    s <- c("He said \"It's all the same.\"",
           "He said 'It's all the same.'")

    s_double <- c("\"He said \\\"It's all the same.\\\"\"",
                  "\"He said 'It's all the same.'\"")
    s_single <- c("'He said \"It\\\'s all the same.\"'",
                  "'He said \\\'It\\\'s all the same.\\\''")

    expect_that(double_quote(s), equals(s_double))
    expect_that(single_quote(s), equals(s_single))
})
